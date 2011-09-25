#lang racket/base

(require racket/list racket/date racket/port racket/promise racket/runtime-path
         file/md5 (for-syntax racket/base))

;; ----------------------------------------------------------------------------

(provide heredir get-opt defines-from-options
         add-options-file get-options-files)

(define-runtime-path heredir ".")

(define options '())
(define options-files '())
;; if it's a list beginning with `+++' then it's a list of multiple option
;; chunks -- this allows sending a single file to the client that holds both
;; the local options and the defaults
(define (add-options opts)
  (if (and (pair? opts) (eq? '+++ (car opts)))
    (for-each add-options (cdr opts))
    (set! options (cons opts options))))
(define (get-options-files) (reverse options-files))
(define (add-options-file file)
  (add-options (call-with-input-file file read))
  (set! options-files (cons (path->complete-path file) options-files)))

;; use source config by default -- on the client this will include both
(add-options-file (build-path heredir "config.rktd"))

(define none (gensym 'no-default))
(define (get-opt #:default [default none] . option-path)
  (define (err) (error 'get-opt "bad option path or file for ~e" option-path))
  (define (search path options)
    (cond [(null? path)
           (if (= 1 (length options)) (car options) (err))]
          [(assq (car path) options)
           => (λ (more)
                (if (pair? more) (search (cdr path) (cdr more)) (err)))]
          [else none]))
  (let loop ([options options])
    (if (null? options)
      (if (eq? default none)
        (error 'get-opt "option not found: ~e" option-path)
        default)
      (let ([r (search option-path (car options))])
        (if (eq? r none) (loop (cdr options)) r)))))

;; define lazily, so later additions to the options will take effect
(define-syntax define-from-options
  (syntax-rules ()
    [(_ name opt ...)
     (begin (define p (lazy (get-opt 'opt ... 'name)))
            (define-syntax name
              (syntax-id-rules (set!)
                [(set! _ . xs) (error 'name "cannot mutate")]
                [(_ . xs) ((force p) . xs)]
                [_ (force p)])))]))
(define-syntax (defines-from-options stx)
  (define ((loop options) x)
    (if (identifier? x)
      (with-syntax ([x x] [(opt ...) options])
        (list #'(define-from-options x opt ...)))
      (syntax-case x ()
        [(opt more ...)
         (apply append (map (loop (cons #'opt options))
                            (syntax->list #'(more ...))))])))
  (with-syntax ([(defn ...)
                 (apply append (map (loop '()) (cdr (syntax->list stx))))])
    #'(begin defn ...)))

;; ----------------------------------------------------------------------------

(provide current-date-string)

(define (current-date-string)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (seconds->date (current-seconds)) #t)))

;; ----------------------------------------------------------------------------

(provide is-master-password?)

(define-from-options master-password)
(define (is-master-password? pswd)
  (define (->bytes s) (if (string? s) (string->bytes/utf-8 s) s))
  (equal? (->bytes master-password) (md5 (->bytes pswd))))

;; ----------------------------------------------------------------------------

(provide get-path-option path-list<?)

(define-from-options path-specs)

(define (get-path-option* path option)
  (define (matching? spec list)
    (if (null? spec)
      (null? list)
      (case (car spec)
        [(?) (and (pair? list) (matching? (cdr spec) (cdr list)))]
        [(*) (or (matching? (cdr spec) list)
                 (and (pair? list) (matching? spec (cdr list))))]
        [else (and (pair? list)
                   (let ([spec1 (car spec)] [x (car list)])
                     (or (equal? spec1 x)
                         (and (regexp? spec1) (regexp-match? spec1 x))))
                   (matching? (cdr spec) (cdr list)))])))
  (define (getopt options)
    (and (pair? options)
         (if (eq? option (car options))
           (cadr options)
           (getopt (cddr options)))))
  (ormap (λ (s+o)
           (and (or (equal? '(*) (car s+o)) ; optimize common case
                    (matching? (car s+o) path))
                (getopt (cdr s+o))))
         path-specs))

(define get-path-option
  (let ([t (make-hasheq)])
    (λ (path option)
      (hash-ref! (hash-ref! t option (λ () (make-hash))) path
                 (λ () (get-path-option* path option))))))

(define string->natural-list
  (let ([t (make-weak-hash)])
    (λ (s)
      (hash-ref! t s
        (λ ()
          (let loop ([ms (regexp-match-positions* #rx"[0-9]+" s)] [i 0])
            (if (null? ms)
              (list (substring s i))
              (list* (substring s i (caar ms))
                     (string->number (substring s (caar ms) (cdar ms)))
                     (loop (cdr ms) (cdar ms))))))))))

(define (natural-string<? s1 s2)
  (let loop ([l1 (string->natural-list s1)] [l2 (string->natural-list s2)])
    ;; the two lists are always (str num str num ... num str)
    (cond [(null? l2) #f]
          [(null? l1) #t]
          [(equal? (car l1) (car l2)) (loop (cdr l1) (cdr l2))]
          [(string? (car l1)) (string<? (car l1) (car l2))]
          [else (< (car l1) (car l2))])))

(define path-list<?
  (let ([t (make-hash)])
    (λ (l1 l2)
      (hash-ref! t (list l1 l2)
        (λ ()
          (let loop ([l1 l1] [l2 l2] [rpath '()])
            (cond
              [(null? l2) #f]
              [(null? l1) #t]
              [(equal? (car l1) (car l2))
               (loop (cdr l1) (cdr l2) (cons (car l1) rpath))]
              [else
               (let* ([get (λ (x key)
                             (get-path-option (reverse (cons x rpath)) key))]
                      [x1 (car l1)]               [x2 (car l2)]
                      [o1 (or (get x1 'order) 0)] [o2 (or (get x2 'order) 0)]
                      [c1 (get x1 'comparator)]   [c2 (get x2 'comparator)]
                      [c (and c1 c2 (equal? c1 c2) (get-comparator c1))]
                      [c (if c (c x1 x2) '?)])
                 (cond [(not (eq? '? c)) c]
                       [(< o1 o2) #t]
                       [(< o2 o1) #f]
                       [else (natural-string<? x1 x2)]))])))))))

;; ----------------------------------------------------------------------------

(provide make-reader make-writer)

(define (make-writer o)
  (define (writer x) (write x o) (newline o) (flush-output o))
  writer)

(define (default-error* fmt . args)
  (raise-user-error (apply format fmt args)))

(define (make-reader i timeout #:error [error* default-error*]
                               #:limit [limit #f])
  (define ch (make-channel))
  ;; write two values for each read, so it doesn't start reading until needed;
  ;; use a list to easily distinguish it from the timeout's #f; and use two
  ;; channels to make it possible to read from multiple threads
  (define reader-thread
    (thread (λ ()
              (define (read*)
                (if limit
                  (with-handlers ([exn:fail:read:eof?
                                   (λ (e) (error* "input overflow"))])
                    (read (make-limited-input-port i (* 1024 limit) #t)))
                  (read i)))
              (define ch2 (make-channel))
              (let loop () ; two loops so we can have a single with-handlers
                (with-handlers ([exn? (λ (e) (channel-put ch2 e) (loop))])
                  (let loop ()
                    (channel-put ch ch2)
                    (channel-put ch2 (list (read*)))
                    (loop)))))))
  (define reader
    (case-lambda
      [() (if (thread-dead? reader-thread)
            (error* "reader input thread is dead")
            (let* ([r (sync/timeout timeout ch)]
                   [r (and r (sync/timeout timeout r))])
              (cond [(exn? r)  (raise r)]
                    [(pair? r) (car r)]
                    [(not r)   (kill-thread reader-thread)
                               (error* "reader timeout!")]
                    [else (error* "internal reader error: ~e" r)])))]
      [(expected)
       (let ([r (reader)])
         (if (equal? r expected)
           r
           (error* "reader: bad reply, expected ~s, got ~e" expected r)))]
      [(expected fmt . args)
       (let ([r (reader)])
         (if (equal? r expected) r (apply error* fmt args)))]))
  reader)

;; ----------------------------------------------------------------------------

(provide (struct-out ping) (struct-out pong))

(define-struct ping (time diffs) #:prefab)
(define-struct pong (uptime messages diffs username password) #:prefab)

;; ----------------------------------------------------------------------------

(define (get-comparator c)
  (case c
    [(qa-comparator) qa-comparator]
    [else (error 'get-comparator "unknown comparator: ~e" c)]))

(define qa-comparator
  (let ()

    (define order '(question answer scratch))

    (define (split str)
      (let* ([m (regexp-match #rx"^([^0-9.]+)([0-9]+)?([^0-9.]*)?(.*)$" str)]
             [n (and m (caddr m))]
             [r (and m (cdddr m))]
             [m (and m (string->symbol (string-downcase (cadr m))))]
             [m (and (memq m order) m)])
        (values (or m (error 'qa-comparator "unknown prefix: ~e" str))
                (and n (string->number n))
                (car r)
                (cadr r))))

    (define (qa-comparator s1 s2)
      ;; Question 1 b .txt
      ;; tttttttt n e rrrr
      (define-values (t1 n1 e1 r1) (split s1))
      (define-values (t2 n2 e2 r2) (split s2))
      (cond [(and (not n1) (not n2)) (string<? s1 s2)]
            [(not n1) #f]
            [(not n2) #t]
            [(< n1 n2) #t]
            [(< n2 n1) #f]
            [(string<? e1 e2) #t]
            [(string<? e2 e1) #f]
            [(memq t1 (cdr (memq t2 order))) #f]
            [(memq t2 (cdr (memq t1 order))) #t]
            [(equal? r1 r2) (error 'qa-comparator "bad inputs: ~e, ~e" s1 s2)]
            [else (string<? r1 r2)]))

    qa-comparator))
