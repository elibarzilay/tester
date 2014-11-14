#!/bin/sh
#| -*- scheme -*-
eval "$($(dirname "$0")/../../utils/sh-init)"

while true; do
  echo ">>> Starting server..."
  "$PLTHOME/bin/racket" "$0" "$@"
  echo ">>> Server died, waiting 1 second..."
  sleep 1
done
|#

#lang racket/base

(require racket/match racket/list racket/string racket/file racket/system
         racket/date mzlib/thread racket/runtime-path openssl
         (for-syntax racket/base)
         "shared.rkt")

;; ----------------------------------------------------------------------------

(defines-from-options
  workdir
  server-name server-port
  [server poll-frequency read-timeout read-limit accept-ips
          logfile stderr prompt
          login-mode password-verifier content-dir clients-dir
          backup-interval backup-command allow-messages?]
  [netboot client-files])

(current-directory workdir)
(when (file-exists? "config.rktd") (add-options-file "config.rktd"))

;; ----------------------------------------------------------------------------

(define verify-password
  (let* ([x (format "passwords-~a.rkt" (car password-verifier))]
         [x (build-path heredir x)]
         [x (with-handlers ([(λ (_) #t) (λ (_) #f)])
              (dynamic-require x 'make-verifier))])
    (if x
      (apply x (cdr password-verifier))
      (error 'verify-password
             "unknown password scheme: ~e" password-verifier))))

;; ----------------------------------------------------------------------------

(define logger-thread
  (thread
   (λ ()
     (define log (and logfile (open-output-file logfile #:exists 'append)))
     (define err (and stderr (current-error-port)))
     (let loop ()
       (define x (thread-receive))
       (define t (current-date-string))
       (when (eq? eof x)
         (when log (close-output-port log))
         (when err (close-output-port err))
         (kill-thread (current-thread)))
       (let ([mode (car x)] [str (format "[~a] ~a\n" t (cdr x))])
         (when (and log (memq mode '(file both)))
           (display str log) (flush-output log))
         (when (and err (memq mode '(out both)))
           (display "| " err) (display str err) (flush-output err)))
       (loop)))))
(define logging-identifier (make-thread-cell #f))
;; first argument can be a mode: 'file or 'out to output only there, or 'both
(define (log fmt/mode . args)
  (let* ([mode? (not (string? fmt/mode))]
         [mode (if mode? fmt/mode   'both)]
         [fmt  (if mode? (car args) fmt/mode)]
         [args (if mode? (cdr args) args)]
         [msg  (apply format fmt args)]
         [msg  (let ([id (thread-cell-ref logging-identifier)])
                 (if id (format "[~a] ~a" id msg) msg))])
    (thread-send logger-thread (cons mode msg))))
(define (shutdown-logger)
  (thread-send logger-thread eof)
  (thread-wait logger-thread))

;; ----------------------------------------------------------------------------

(define client-files+contents
  (let ()
    (define-struct file (name path time text) #:mutable)
    (define (file->list file) (list (file-name file) (file-text file)))
    (define (update-options opt-file)
      (define paths (file-path opt-file))
      (let ([time (map file-or-directory-modify-seconds paths)])
        (unless (equal? time (file-time opt-file))
          (when (file-time opt-file) (log "config modified"))
          (set-file-time! opt-file time)
          (let* ([txt (add-between (map file->bytes paths) #"\n")]
                 [txt (apply bytes-append txt)]
                 [txt (bytes-append #"(+++\n" txt #"\n)\n")])
            (set-file-text! opt-file txt))))
      opt-file)
    (define contents
      (append (for/list ([name (in-list client-files)])
                (let* ([path (build-path heredir name)]
                       [time (file-or-directory-modify-seconds path)])
                  (make-file name path time (file->bytes path))))
              (list (make-file "config.rktd" (get-options-files) #f #f))))
    (define last-poll (current-inexact-milliseconds))
    (define (get)
      (define now (current-inexact-milliseconds))
      (when ((- now last-poll) . > . 1000)
        (set! last-poll now)
        (for ([file (in-list contents)])
          (define path (file-path file))
          (if (list? path)
            (update-options file)
            (let ([newtime (file-or-directory-modify-seconds path)])
              (unless (equal? newtime (file-time file))
                (log "source file modified: ~a" (file-name file))
                (set-file-time! file newtime)
                (set-file-text! file (file->bytes path)))))))
      (map file->list contents))
    get))

;; ----------------------------------------------------------------------------

(define-syntax-rule (with-sema sema expr ...)
  (call-with-semaphore sema (λ () expr ...)))

(define-struct client (thread ip id username) #:mutable)
(define clients (make-hash)) ; maps id to client
(define clients-sema (make-semaphore 1))
(define cached-all-clients #f)
(define (all-clients)
  (or cached-all-clients
      (let ([all (sort (for/list ([(k v) (in-hash clients)]) v)
                       string<? #:key client-id)])
        (set! cached-all-clients all)
        all)))

;; old-ids maps an old id to a new one, this is used for two reasons:
;; * the controller might use an id just when it changes
;; * the client might send a poll request just when it changes
;; in both cases it's best to silently use the new id instead of barfing: the
;; controller sees what's going on anyway, and it's harmless if we end up
;; sending a poll to the wrong client (but note that this is *not* searched
;; recursively, which is fine for both cases)
(define old-ids (make-hash))
(define (id->client id)
  (or (hash-ref clients id #f)
      (let ([id (hash-ref old-ids id #f)])
        (and id (hash-ref clients id #f)))))

(define content
  (let ()
    (define-struct file (path-name path-list time [text #:mutable]))
    (define sep-rx (if (eq? 'windows (system-type)) #rx"[/\\]" #rx"/"))
    (define (path->file path read-text?)
      (let* ([name (path->string path)]
             [list (regexp-split sep-rx name)]
             [inc? (not (get-path-option list 'ignore))]
             [time (and inc? (file-or-directory-modify-seconds path))]
             [text (and inc? read-text? (file->string path))])
        (and inc? (make-file name list time text))))
    (define (get read-text?)
      (sort (fold-files (λ (path kind acc)
                          (if (eq? 'file kind)
                            (let ([file (path->file path read-text?)])
                              (if file (cons file acc) acc))
                            ;; directories: descend, dead symlinks: ignore
                            acc))
                        '())
            path-list<? #:key file-path-list))
    (define (file->list file [more '()])
      (list* (file-path-list file) (file-text file) more))
    (define (diff)
      (let loop ([prev current] [next (get #f)] [diff '()] [new '()])
        (define (deleted)
          (log "content file deleted: ~a" (file-path-name (car prev)))
          (let ([diff (cons (list 'deleted (file-path-list (car prev))) diff)])
            (loop (cdr prev) next diff new)))
        (define (added/modified added?)
          (define file (car next))
          (log "content file ~a: ~a" (if added? 'added 'modified)
               (file-path-name file))
          (set-file-text! file (file->string (file-path-name file)))
          (let ([prev (if added? prev (cdr prev))]
                [diff (cons (cons (if added? 'added 'changed)
                                  (file->list file))
                            diff)])
            (loop prev (cdr next) diff (cons file new))))
        (define (done)
          (set! current (reverse new))
          (set! current-list (map file->list current))
          (reverse diff))
        (cond [(and (null? prev) (null? next)) (done)]
              [(null? next) (deleted)]
              [(null? prev) (added/modified #t)]
              [(path-list<? (file-path-list (car next))
                            (file-path-list (car prev)))
               (added/modified #t)]
              [(path-list<? (file-path-list (car prev))
                            (file-path-list (car next)))
               (deleted)]
              [else (if (equal? (file-time (car prev)) (file-time (car next)))
                      (loop (cdr prev) (cdr next) diff (cons (car prev) new))
                      (added/modified #f))])))
    ;; merge a file-list (see the format of `current-list' below) with the
    ;; contents of a user directory
    (define (merge-with-local current-list save-dir)
      (define local
        (if (directory-exists? save-dir)
          (parameterize ([current-directory save-dir])
            (map (λ (x) (file->list x '(#t))) (get #t)))
          '()))
      (define (merge list1 list2)
        (cond [(null? list1) list2]
              [(null? list2) list1]
              [(path-list<? (caar list1) (caar list2))
               (cons (car list1) (merge (cdr list1) list2))]
              [(path-list<? (caar list2) (caar list1))
               (cons (car list2) (merge list1 (cdr list2)))]
              ;; if an item is on both, use the one from list1
              [else (cons (car list1) (merge (cdr list1) (cdr list2)))]))
      (merge local current-list))
    ;; current is a list of file values
    (define current
      (parameterize ([current-directory content-dir]) (get #t)))
    ;; current-list is a list of (<path-list> <contents>)
    (define current-list (map file->list current))
    ;; 'get  -> returns the current list, each is (<path-list> <contents>)
    ;; 'diff -> returns diffs from the previous list (and update the held list)
    ;;    each diff is (added <path-list> <contents>),
    ;;              or (changed <path-list> <contents>),
    ;;              or (deleted <path-list>)
    (define (content msg [arg #f])
      (case msg
        [(get)  (if arg (merge-with-local current-list arg) current-list)]
        [(diff) (parameterize ([current-directory content-dir]) (diff))]))
    content))

;; filter the result of `content' above -- if it's a diff, it will have a
;; symbol in its car and the path-list in its cadr, otherwise the path-list is
;; in the car.  `pred?' can be one of the below predicates.
(define (filter-content content/diff pred?)
  (cond [(null? content/diff) '()]
        [(symbol? (caar content/diff)) ; filter a diff
         (filter (λ (x) (pred? (cadr x))) content/diff)]
        [else ; filter a normal content
         (filter (λ (x) (pred? (car x))) content/diff)]))
(define (editable? path-list) (get-path-option path-list 'editable))
(define (readonly? path-list) (not (get-path-option path-list 'editable)))

;; ----------------------------------------------------------------------------

;; Communication is all derived by the server, and all information from the
;; client is passed as a response to ping commands.  When the client wants to
;; initiate sending information (for example, and alert message), it will
;; initiate a new connection with the only purpose of trigerring such a ping.

(define (client-send client data)
  (thread-send (client-thread client) data void))

(define good-ip?
  (let ([prefix->pred
         (λ (pfx)
           (let* ([rx (regexp-replace #rx"\\.$" pfx "")]
                  [rx (regexp-replace* #rx"\\." rx "\\\\.")]
                  [rx (regexp (string-append "^" rx "(?:\\.|$)"))])
             (λ (ip) (regexp-match? rx ip))))])
    (cond [(not accept-ips) (λ (ip) #t)]
          [(string? accept-ips) (prefix->pred accept-ips)]
          [else (let ([preds (map prefix->pred accept-ips)])
                  (λ (ip) (ormap (λ (p) (p ip)) preds)))])))

(define top-custodian (current-custodian))

(define (run-client i o)
  (define (error* fmt . args) (raise-user-error (apply format fmt args)))
  (define ->client (make-writer o))
  (define client->
    (make-reader i read-timeout #:error error* #:limit read-limit))
  (define ip
    (let-values ([(_ ip) (ssl-addresses i)])
      (thread-cell-set! logging-identifier ip)
      ip))
  (define id #f)
  (define logged-in? (not login-mode))
  (define save-dir #f)
  (define this-client #f)
  (define (set-id! new-id reason)
    (if (equal? id new-id)
      (log "ignoring identical id (from ~a)" id)
      (begin
        (with-sema clients-sema
          (when id (hash-remove! clients id))
          (when (hash-ref clients new-id #f)
            (error* "client-id (from ~a) exists: ~e" reason id))
          (case reason
            [(connection)
             (log "connection from ~a" ip)
             (set! this-client
                   (make-client (current-thread) new-id new-id #f))]
            [(client)
             (log "-> ~a (client id received)" new-id)
             (set-client-id! this-client new-id)]
            [(login forced-login)
             (log "-> ~a (~a)" new-id (if (eq? 'forced-login reason)
                                        "forced login" "login accepted"))
             (set-client-username! this-client new-id)]
            ;; [(forced-logout)
            ;;  (log "-> ~a (forced logout)" new-id)]
            [else (error* "internal error: bad reason for `set-id!'; ~e"
                          reason)])
          (when id (hash-set! old-ids id new-id))
          (hash-set! clients new-id this-client)
          (set! cached-all-clients #f))
        (set! id new-id)
        (set! save-dir (build-path clients-dir id))
        (thread-cell-set! logging-identifier new-id))))
  (define last-poll-uptime 0)
  (define (die!)
    (with-sema clients-sema
      (hash-remove! clients id)
      (set! cached-all-clients #f))
    (close-input-port i) (close-output-port o)
    (parameterize ([current-custodian top-custodian])
      (kill-thread (current-thread))))
  (define (send/ok msg . args)
    (->client msg)
    (for ([arg (in-list args)]) (->client arg))
    (client-> 'ok "client did not ok a ~e message" msg))
  (define (login username forced?)
    (set-id! username (if forced? 'forced-login 'login))
    (thread-send (current-thread) 'post-login)
    (set! logged-in? #t))
  (define (poll time diff)
    (->client 'ping)
    (->client (make-ping time
                         (if logged-in? diff (filter-content diff readonly?))))
    (client-> 'pong "client died")
    (let ([pong (client->)])
      (unless (pong? pong) (error* "poll: got a bad pong message: ~e" pong))
      ;; check client uptime as a safety measure
      (if ((pong-uptime pong) . >= . last-poll-uptime)
        (set! last-poll-uptime (pong-uptime pong))
        (log "*** got bad poll time"))
      ;; check username/password if needed
      (unless logged-in?
        (let ([user (pong-username pong)] [pswd (pong-password pong)])
          (when (and user pswd)
            (if (or (is-master-password? pswd) (verify-password user pswd))
              (begin (log "client authenticated") (login user #f))
              (log "*** bad client password for ~a" user)))))
      ;; save edits first (eg, before quitting) only if logged in
      (when logged-in?
        (for ([diff (pong-diffs pong)])
          (match diff
            [(list (list dirpart ... file) text)
             (define dir (apply build-path save-dir dirpart))
             (make-directory* dir)
             (call-with-output-file (build-path dir file) #:exists 'replace
               (λ (o) (display text o) (flush-output o)))])))
      ;; show messages and possibly act accordingly -- even if not logged in
      ;; (since the client application might send an alert that the controller
      ;; should know about)
      (for ([msg (pong-messages pong)])
        (match msg
          [(list 'login) (void)] ; sent after user+pswd read, no info
          [(list 'message msg)
           (for ([line (regexp-split #rx"(?: *\n)+"
                                     (regexp-replace " *\n+$" msg ""))])
             (log "says: ~a" line))]
          [(list 'alert msg) (log "*** client alert: ~a" msg)]
          [(list 'quit) (log "*** client quitting") (->client 'ok) (die!)]
          [else (log "*** client sent a bad message: ~e" msg)])))
    (->client (if logged-in? 'ok 'bad-password)))
  ;; don't `set-id!' here; it will show polls and conflict with the real client
  (with-handlers ([exn? (λ (e)
                          (log "*** client-error: ~a" (exn-message e))
                          (unless exn:fail:user?
                            ((error-display-handler) (exn-message e) e))
                          (die!))])
    (unless (good-ip? ip)
      (error* "got a connection from an invalid ip: ~a" ip))
    (let ([init (client->)])
      (case init
        [(tester-client-connection) (set-id! ip 'connection) (->client 'ok)]
        [(tester-get-client-files) (->client (client-files+contents)) (die!)]
        [(tester-client-do-poll)
         ;; no need to verify that we're getting a correct id, and safe to
         ;; ignore an inexistent one
         (let* ([id (client->)] [c (id->client id)])
           (if c
             (client-send c '(poll #f ()))
             (log "poll request for bad client: ~a" id))
           (die!))]
        [else (error* "got a bad connection token: ~e" init)]))
    (->client 'send-client-id)
    (let ([id (client->)])
      (unless (string? id) (error* "bad value for client-id: ~e" id))
      (unless (equal? "" id) (set-id! id 'client)))
    (log "client connected")
    (->client 'ok)
    (send/ok 'set-id id) ; in case the client didn't have an id
    ;; send the path-list and the text content of all files
    (send/ok 'content (let ([c (content 'get save-dir)])
                        (if logged-in? c (filter-content c readonly?))))
    (cond [(not logged-in?) (send/ok 'login-required)]
          [allow-messages? (send/ok 'enable-messages #t)])
    ;; do an immediate poll, in case the server is down and the client has
    ;; stuff to say (like sending a user+paswd)
    (poll #f '())
    (let loop ()
      (define x (thread-receive))
      (match x
        ['kill    (log "killing client")    (->client 'die) (die!)]
        ['unlock  (log "unlocking client")  (send/ok 'unlock)]
        ['restart (log "restarting client") (send/ok 'restart) (die!)]
        [(list 'freeze switch? b?)
         (when switch? (send/ok 'show-blank b?))
         (log "~afreezing~a client"
              (if b? "" "un") (if switch? " and blanking" ""))
         (send/ok 'freeze b?)]
        [(list 'enable-messages b?)
         (log "~aabling messages" (if b? "en" "dis"))
         (send/ok 'enable-messages b?)]
        [(list 'message switch? string)
         (when switch? (send/ok 'show-interaction))
         (send/ok 'message string)]
        [(list 'login username)
         (if logged-in?
           (log "ignoring forced login (already logged in)")
           (begin (log "forced login as `~a'" username) (login username #t)))]
        ['post-login
         (send/ok 'set-id+login id)
         (send/ok 'editable-content
                  (filter-content (content 'get save-dir) editable?))
         (when allow-messages? (send/ok 'enable-messages #t))]
        ;; doesn't really work -- need to reset line reading, but also a
        ;; bunch of other state like the contents of the interaction window
        ;; ['logout
        ;;  (poll #f '()) ; sync possible changes
        ;;  (set-id! (client-id this-client) 'forced-logout)
        ;;  (send/ok 'set-id id)
        ;;  (send/ok 'enable-messages #f)
        ;;  (send/ok 'logout)]
        [(list 'poll time diff)
         (poll time diff)])
      (loop))))

(define (run-test-server)
  (log "server starting")
  (run-server server-port
              run-client
              #f ; clients should never disconnect
              (λ (exn)
                (log "connection error: ~a"
                     (if (exn? exn) (exn-message exn) exn)))
              (λ (port-k cnt reuse?)
                (let ([l (ssl-listen port-k cnt #t)])
                  (ssl-load-certificate-chain! l "server-cert.pem")
                  (ssl-load-private-key! l "private-key.pem")
                  l))
              ssl-close
              ssl-accept
              ssl-accept/enable-break))

;; ----------------------------------------------------------------------------

(define next-backup-time
  (inexact->exact (round (current-inexact-milliseconds))))
(define backup-command*
  (and backup-command
       (let ([exe (find-executable-path (car backup-command))])
         (if exe
           (cons exe (cdr backup-command))
           (begin (log "*** backup command not found, using racket: ~a"
                       (car backup-command))
                  #f)))))

(define (do-backup)
  (when (and backup-interval
             ((current-inexact-milliseconds) . > . next-backup-time))
    (set! next-backup-time
          (+ next-backup-time (* 1000 backup-interval)))
    (let ([backup-dir
           (format "~a-~a" clients-dir
                   (regexp-replace* #rx"[^0-9]+" (current-date-string) ""))])
      (cond
        [(directory-exists? backup-dir)
         (log "*** chosen backup directory exists, skipping: ~a" backup-dir)]
        [backup-command*
         (let ([r (apply system*/exit-code
                         `(,@backup-command* ,clients-dir ,backup-dir))])
           (unless (zero? r)
             (log "*** backup command returned an error code (~a)" r)))]
        [else (copy-directory/files clients-dir backup-dir)]))))

;; this can be 'current to show the current time, a fixed string to be shown
;; as-is, or an integer which will show a count down to that time (expressed as
;; the result of current-seconds).
(define reported-time 'current)
(define (format-minutes M)
  (let* ([neg? (< M 0)] [M (abs M)] [H (quotient M 60)] [M (modulo M 60)])
    (format "~a~a:~a~a" (if neg? "-" "") H (if (< M 10) "0" "") M)))
(define (format-time date/secs)
  (let ([date (if (number? date/secs) (seconds->date date/secs) date/secs)])
    (format-minutes (+ (date-minute date) (* 60 (date-hour date))))))
(define (get-time)
  (define (pad5 str)
    (string-append
     (case (string-length str)
       [(0) "     "] [(1) "    "] [(2) "   "] [(3) "  "] [(4) " "] [else ""])
     str))
  (pad5
   (cond [(string? reported-time) reported-time]
         [(number? reported-time)
          (format-minutes (round (/ (- reported-time (current-seconds)) 60)))]
         ;; (eq? 'current reported-time) -- show current time by default
         [else (format-time (current-seconds))])))
(define (set-time str)
  (define s->n string->number)
  (define (show)
    (printf "the time is set to: ~a\n"
            (cond [(number? reported-time)
                   (format "countdown to ~a (~a)"
                           (format-time reported-time) (get-time))]
                  [(string? reported-time) (format "~s" reported-time)]
                  [else (format "current time (~a)" (get-time))])))
  (define help
    '("current -- use the current time (on the server)"
      "\"...\"   -- set a fixed string for the time (5 chars are visible)"
      "HH:MM   -- set timer to count down to the given time"
      "Ah Bm   -- set timer to A hrs and B mins from now (can omit one)"
      "show    -- show the current time setting"
      "Note: in countdown mode nothing happens when the countdown is reached,"
      "      you need to lock the clients yourself"))
  (define new
    (match str
      ["help" (for ([s help]) (printf "  ~a\n" s)) #f]
      [(regexp #rx"^(?i:cur(?:r(?:e(?:n(?:t)?)?)?)?)$") 'current]
      [(regexp #rx"^\"(.*)\"$" (list _ s)) s]
      [(regexp #rx"^([0-9][0-9]?):([0-9][0-9]?)$" (list _ H M))
       (let ([d (seconds->date (current-seconds))])
         (find-seconds 0 30 8 (date-day d) (date-month d) (date-year d)))]
      [(regexp #rx"^([0-9]+)[hH]$" (list _ H))
       (+ (current-seconds) (* 60 60 (s->n H)))]
      [(regexp #rx"^([0-9]+)[mM]$" (list _ M))
       (+ (current-seconds) (* 60 (s->n M)))]
      [(regexp #rx"^([0-9]+)[hH] ([0-9]+)[mM]$" (list _ H M))
       (+ (current-seconds) (* 60 60 (s->n H)) (* 60 (s->n M)))]
      ["show" (show) #f]
      [_ (error "unknown time format (use `help' for more info)")]))
  (when (and new (not (equal? new reported-time)))
    (set! reported-time new)
    (show)))

(define (run-poller)
  (define-values [clients poll+diff]
    ;; need to sync, so we always send valid diffs to clients
    (with-sema clients-sema
      (values (all-clients) (list 'poll (get-time) (content 'diff)))))
  (for ([c (in-list clients)]) (client-send c poll+diff))
  (do-backup)
  (sleep poll-frequency)
  (run-poller))

;; ----------------------------------------------------------------------------

(define commands '())
(define-struct command (form help handler))
(define no-match (gensym))
(define-syntax defcommand
  (syntax-rules ()
    [(defcommand (x ...) #:form form help body ...)
     (let ([handler
            (match-lambda
             [(list x ...)
              (with-handlers ([exn?
                               (λ (e) (printf "error: ~a\n" (exn-message e)))])
                body ...)]
             [_ no-match])])
       (set! commands (cons (make-command form help handler) commands)))]
    [(defcommand (x ...) help body ...)
     (defcommand (x ...) #:form '(x ...) help body ...)]))

(defcommand () #f (void))
(defcommand (cmd _ ...) #f (printf "Unknown command or bad arguments\n"))

(define last-vals (make-hasheq))
(define (w/last name val)
  (if (equal? val "^")
    (or (hash-ref last-vals name #f)
        (error 'with-last "no value set for `~s'" name))
    (begin (hash-set! last-vals name val) val)))

(defcommand ("help")
  "show this help"
  (printf "Comamnds:\n")
  (for ([cmd (in-list (reverse commands))])
    (when (command-help cmd)
      (printf "  ")
      (let ([form (command-form cmd)])
        (if (string? form)
          (printf "~a " form)
          (for ([x (in-list form)])
            (cond [(and (symbol? x) (not (eq? '... x))) (printf "<~a> " x)]
                  [(and (pair? x) (eq? (car x) 'on/off)) (printf "{on|off} ")]
                  [else (printf "~a " x)]))))
      (printf "-- ~a\n" (command-help cmd))))
  (printf "- use `-all' for <id> to apply to all clients\n")
  (printf "- use `^' to get the last command, id, or text\n"))

(defcommand ("list")
  "show connected clients"
  (let ([all (all-clients)])
    (if (null? all)
      (printf "No connected clients\n")
      (begin (printf "Connected clients:\n")
             (for ([m (in-list all)])
               (let ([ip (client-ip m)]
                     [id (client-id m)]
                     [u  (client-username m)])
                 (printf "  ~a~a [~a]\n"
                         (if u (format "~a @ " u) "") (or id ip) ip)))))))

(defcommand ((and (regexp ";+") fst) any ...)
  #:form ";..." "leave a comment in the log"
  ;; all controller commands are shown on the logfile anyway
  (log 'out "~a" (string-join (cons fst any) " ")))

(define (send-to id msg)
  (let ([id (w/last 'id id)])
    (if (equal? "-all" id)
      (for ([c (in-list (all-clients))]) (client-send c msg))
      (client-send (or (id->client id)
                       (error (format "No such client `~a'" id)))
                   msg))))

(define-syntax-rule (defcommand/send-to (cmd arg ...) help expr)
  (defcommand (cmd id arg ...) help (send-to id expr)))

(define-match-expander on/off
  (syntax-rules ()
    [(_ x)
     (app (λ (x) (list x (equal? x "on"))) (list (or "on" "off") x))]))

(defcommand/send-to ("tell" msg ...)
  "show a message on client"
  (list 'message #f (w/last 'text (string-join msg " "))))
(defcommand/send-to ("tell!" msg ...)
  "switch to the interaction window and show a message"
  (list 'message #t (w/last 'text (string-join msg " "))))
(defcommand/send-to ("messages" (on/off b?))
  "(dis)allow client to send messages"
  (list 'enable-messages b?))
(defcommand ("set-time" time ...)
  "set the time display (`set-time help' for more info)"
  (set-time (string-join time " ")))
(defcommand/send-to ("freeze" (on/off b?))
  "(un)freeze client (no input)"
  (list 'freeze #f b?))
(defcommand/send-to ("freeze!" (on/off b?))
  "(un)freeze client and blank editor"
  (list 'freeze #t b?))
(defcommand/send-to ("login" username)
  "make a client log in as username"
  (list 'login username))
;; don't use this -- the client needs to reset the currently entered line, but
;; even with that there are a bunch of other problems like resetting the
;; messages window etc.  Easier to just restart the client.
;; (defcommand/send-to ("logout")
;;   "force client to log out"
;;   'logout)
(defcommand/send-to ("unlock")
  "unlock client"
  'unlock)
(defcommand/send-to ("restart")
  "restart client"
  'restart)
(defcommand/send-to ("kill")
  "terminate client"
  'kill)

(defcommand ("make-netboot")
  "create a boot file to run on clients"
  (defines-from-options
    [netboot file client-dir racket-path batch-prefix batch-loop?])
  (define (echo-quote s) (regexp-replace* #rx"[ <>|\"^]" s "^\\0"))
  (define gracket-paths
    (for/list ([p (if (list? racket-path) racket-path (list racket-path))])
      (let* ([p (string-append p "/gracket-text.exe")]
             [p (regexp-replace* #rx"/" p "\\\\")])
        p)))
  (define text
    (let* ([text `("#lang racket"
                   (define server ,server-name)
                   (define port ,server-port)
                   (define client-dir ,client-dir)
                   ,(file->string
                     (build-path heredir "netboot-template.rktl")))]
           [text (map (λ (x) (if (string? x) x (format "~s" x))) text)]
           [text (string-append* (add-between text "\n"))])
      text))
  (define (generate-racket)
    (display text))
  (define (generate-batch)
    (let* ([racket-file (regexp-replace #rx"[.][^.]+$" file ".rkt")]
           [text (regexp-match* #rx"[^\n]+" text)]
           [text (map (λ (x) (string-append "echo " (echo-quote x)
                                            ">> " racket-file))
                      text)]
           [execN (let ([n 0]) (λ () (set! n (add1 n)) (format "EXEC~a" n)))]
           [text
            `("@echo off"
              ,@(if (null? batch-prefix) `() `("" ,@batch-prefix))
              ,@(if batch-loop? `("" ":RUNLOOP") `())
              ""
              ("if exist \"~a\\nul\" del /F /Q \"~a\"" ,client-dir ,client-dir)
              ("if exist \"~a\\nul\" rmdir \"~a\"" ,client-dir ,client-dir)
              ""
              ("if exist \"~a\" del \"~a\"" ,racket-file ,racket-file)
              ""
              ,@text
              ""
              ,@(append-map (λ (p)
                              (define label (execN))
                              `(("if not exist \"~a\" goto ~a" ,p ,label)
                                ("  \"~a\" \"~a\"" ,p ,racket-file)
                                ("  goto EXECDONE")
                                (":~a" ,label)))
                            gracket-paths)
              "  echo ERROR: No gracket-text.exe executable found, tried:"
              ,@(map (λ (p) `("  echo ... ~a" ,(echo-quote p))) gracket-paths)
              ":EXECDONE"
              ""
              "sleep 1"
              ""
              ("del /F /Q \"~a\"" ,client-dir)
              ("rmdir \"~a\"" ,client-dir)
              ""
              "pause"
              ,@(if batch-loop? `("" "goto RUNLOOP") `()))])
      (for ([line (in-list text)])
        (printf "~a\r\n" (if (string? line) line (apply format line))))))
  (printf "Creating ~s\n" file)
  (with-output-to-file file #:exists 'truncate
    (cond [(regexp-match? #rx"[.]rkt$" file) generate-racket]
          [(regexp-match? #rx"[.]bat$" file) generate-batch]
          [else (error 'make-netboot "bad file extension in configuration")])))

(define (controller)
  (when prompt (display prompt) (flush-output))
  (with-handlers ([exn:break? (λ (_) (printf "aborting...\n"))])
    (let ([line (read-line)])
    (when (string? line)
      (log 'file "controller: ~a" line)
      (let ([cmd+args (match (regexp-match* #px"\\S+" line)
                        [(cons cmd args) (cons (w/last 'command cmd) args)]
                        [x x])])
        (for/or ([cmd (in-list commands)])
          (let ([r ((command-handler cmd) cmd+args)])
            (not (eq? r no-match)))))
      (controller)))))

;; ----------------------------------------------------------------------------

(define server-thread (thread run-test-server))
(define poller-thread (thread run-poller))

(controller)
(log "server terminating")
(kill-thread server-thread)
(shutdown-logger)
(printf "bye\n")
