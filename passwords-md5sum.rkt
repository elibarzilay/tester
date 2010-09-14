#lang racket/base

(provide make-verifier)

(require file/md5)

(define (->symbol x)
  (cond [(symbol? x) x]
        [(string? x) (string->symbol x)]
        [(bytes?  x) (->symbol (bytes->string/utf-8 x))]
        [else (error '->symbol "cannot convert to a symbol: ~e" x)]))

(define (->bytes x)
  (cond [(bytes?  x) x]
        [(string? x) (string->bytes/utf-8 x)]
        [(symbol? x) (->bytes (symbol->string x))]
        [else (error '->bytes "cannot convert to a byte string: ~e" x)]))

(define (make-verifier password-file)
  (define md5sums
    (for/hash ([entry (in-list (call-with-input-file password-file read))])
      (values (->symbol (car entry)) (->bytes (cadr entry)))))
  (define (verify username password)
    (equal? (md5 (->bytes password))
            (hash-ref md5sums (->symbol username) #f)))
  verify)
