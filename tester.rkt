#lang racket/base

;; ----------------------------------------------------------------------------

(require ffi/unsafe racket/list racket/string racket/file racket/match
         openssl racket/gui/base racket/class framework mrlib/hierlist
         "shared.rkt")

(define (dprintf fmt . args)
  (apply printf fmt args)
  (flush-output)
  (when app-locked? (sleep (* (random) 0.4))))

(defines-from-options
  server-name server-port
  [client app-locked? read-timeout id-file-directories ask-for-client-id?
          show-clock? buffer-limit default-font-size
          min-font-size max-font-size message-font clock-font toc-item-style
          status-color alert-color message-color unsynced-color
          unlocal-editable-color unseen/new-color unread-messages-color])

(define max-buffer-size (* buffer-limit 1024))

;; ----------------------------------------------------------------------------

;; A list of functions that get a boolean flag: #t = lock
(define lockers '())
(define is-locked? #f)
(define (add-locker locker) (set! lockers (cons locker lockers)))
(define (set-global-locked-mode! b)
  (when app-locked?
    (let ([b (and b #t)])
      (unless (equal? is-locked? b)
        (for ([l (reverse lockers)]) (l b))
        (set! is-locked? b)))))

(add-locker
 (λ (lock?)
   (dprintf "~aing application globally\n" (if lock? 'Lock 'Unlock))))

(let ([o (exit-handler)])
  (exit-handler (λ xs
                  (dprintf "Shutting down process...\n")
                  (set-global-locked-mode! #f)
                  (apply o xs))))

(let ([old (uncaught-exception-handler)])
  (uncaught-exception-handler
   (λ (e)
     ((error-display-handler) (exn-message e) e)
     (fprintf (current-error-port) "Aborting!\n")
     (exit 1)
     (old e)))) ; just in case

;; make the framework do it too, just in case
(void (exit:insert-on-callback (λ () (set-global-locked-mode! #f))))

;; ----------------------------------------------------------------------------

(define windows? (eq? 'windows (system-type)))

(define-syntax defwin
  (syntax-rules ()
    [(defwin name [type ...]) (defwin name #f [type ...])]
    [(defwin name lib [type ...])
     (define name
       (if (and app-locked? windows?)
         (get-ffi-obj 'name lib (_fun type ...)) void))]))

(defwin GetFocus             [-> _int32])
(defwin GetForegroundWindow  [-> _pointer])
(defwin GetWindowTextLengthA [_pointer -> _int])
(defwin GetWindowTextA       [_pointer _bytes _int -> _int])
(defwin GetParent            [_pointer -> _pointer])
(defwin GetWindowThreadProcessId [_pointer [_int32 = 0] -> _int32])
(defwin SetForegroundWindow  [_pointer -> _bool])
(defwin GetLastError         [-> _int32])
(define (GetWindowTitle hwnd)
  (define len (GetWindowTextLengthA hwnd))
  (define buf (and (len . > . 0) (make-bytes (add1 len))))
  (if buf
      (begin (GetWindowTextA hwnd buf (add1 len)) (subbytes buf 0 len))
      "<unknown-title>"))

;; http://www.codeproject.com/KB/winsdk/AntonioWinLock.aspx
(define winlock (and app-locked? windows? (ffi-lib "WinLockDll.dll")))
(define-syntax-rule (defwinlock name [type ...])
  (begin (defwin name winlock [type ...])
         (add-locker (λ (locked?) (name (not locked?))))))
;; This one is problematic
;; (defwinlock Desktop_Show_Hide            [_bool -> _void])
(defwinlock StartButton_Show_Hide        [_bool -> _void])
(defwinlock Taskbar_Show_Hide            [_bool -> _void])
(defwinlock Clock_Show_Hide              [_bool -> _void])
(defwinlock Keys_Enable_Disable          [_bool -> _void])
(defwinlock AltTab1_Enable_Disable       [_bool -> _void])
(defwinlock AltTab2_Enable_Disable       [[_int32 = 0] _bool -> _void])
(defwinlock TaskSwitching_Enable_Disable [_bool -> _void])
(defwinlock TaskManager_Enable_Disable   [_bool -> _void])
(defwinlock CtrlAltDel_Enable_Disable    [_bool -> _void])
;; (defwin Process_Desktop winlock [_string _path -> _void])

;; Hack a "show desktop" function
;; (but don't use it, it tends to make a mess)
;; (define (ToggleDesktop)
;;   (define file (make-temporary-file "~a.scf"))
;;   (call-with-output-file file #:exists 'truncate
;;     (λ (o)
;;       (display #"[Shell]\r\nCommand = 2\r\nIconFile=explorer.exe,3\r\n" o)
;;       (display #"[Taskbar]\r\nCommand = ToggleDesktop\r\n" o)))
;;   (shell-execute #f (path->string file) "" (current-directory) 'sw_hide)
;;   (sleep 0.5) ; let it do its work
;;   (delete-file file))

;; Disable the screen saver
(defwin SystemParametersInfoA [[uiAction : _uint]
                               [uiParam  : _uint]
                               [pvParam  : _pointer]
                               [fWinIni  : _uint]
                               -> _bool])
(add-locker (λ (locked?)
              (SystemParametersInfoA 17 ; SPI_SETSCREENSAVEACTIVE
                                     (if locked? 0 1)
                                     #f
                                     0))) ; use SPIF_SENDCHANGE?

;; ----------------------------------------------------------------------------

(current-directory heredir)

(define client-id-file "TESTER-CLIENT-ID")
(define global-client-id #f)
(define (get-saved-client-id)
  (or global-client-id
      (for/or ([dir (in-list id-file-directories)])
        (define file (build-path dir client-id-file))
        (and (file-exists? file)
             (with-handlers ([exn? (λ (_) #f)])
               (call-with-input-file file read-line))))))
(define (save-client-id id)
  (define file
    (or (for/or ([dir (in-list id-file-directories)])
          (define file (build-path dir client-id-file))
          (and (file-exists? file) file))
        (for/or ([dir (in-list id-file-directories)])
          (and (directory-exists? dir) (build-path dir client-id-file)))
        ;; if there is no place to save it, then just don't save it
        ;; (error 'save-client-id "could not find a directory to save in")
        ))
  (when file
    (call-with-output-file file #:exists 'truncate (λ (o) (display id o)))))

(define (make-hasher fun)
  (define t (make-hash))
  (λ (xs) (hash-ref! t xs (λ () (fun xs)))))

(define spec->font
  (make-hasher (match-lambda
                [(list size family style weight)
                 (make-object font% size family style weight)])))

(define spec->color
  (make-hasher
   (λ (color)
     (apply make-object color% (if (list? color) color (list color))))))

(define spec->style
  (make-hasher
   (match-lambda
    [(list size family style weight bgcolor)
     (define st (if size
                  (make-object style-delta% 'change-size size)
                  (make-object style-delta% 'change-nothing)))
     (when family (send st set-family family))
     (when style  (send st set-style-on style))
     (when weight (send st set-weight-on weight))
     (when bgcolor (send st set-delta-background (spec->color bgcolor)))
     st])))

(define fgcolor->style
  (make-hasher
   (λ (fgcolor)
     (define st (make-object style-delta% 'change-nothing))
     (send st set-delta-foreground (spec->color fgcolor))
     st)))

(define threads '())
(define (thread* thunk)
  (define t (thread thunk)) (set! threads (cons t threads)) t)
(define (kill-threads)
  (for ([t (begin0 threads (set! threads '()))]) (kill-thread t)))

;; ----------------------------------------------------------------------------

;; no preference file: use only an in-memory hash table
(define (reset-preferences)
  (define t (make-hasheq))
  (preferences:low-level-get-preference
   (λ (sym [dflt (λ () #f)]) (hash-ref t sym dflt)))
  (preferences:low-level-put-preferences
   (λ (prefs vals)
     (for ([pref (in-list prefs)] [val (in-list vals)])
       (hash-set! t pref val))))
  ;; set some defaults: no exit quesion, anchored search
  (preferences:set 'framework:verify-exit #f)
  (preferences:set 'framework:anchored-search #t))

;; ----------------------------------------------------------------------------

(define-syntax defclass
  (syntax-rules (< :)
    [(defclass c < s : i body ...) (define c (class* s i body ...))]
    [(defclass c < s     body ...) (define c (class  s   body ...))]))

;; ----------------------------------------------------------------------------

(defclass tester-text% < ((compose text:first-line-mixin
                                   text:searching-mixin
                                   text:info-mixin)
                          scheme:text%)
                       : (editor:file<%>) ; to get a "read-only" indicator
  (init-field
   frame
   [path     #f] ; the path of this file (list of strings)
   [text     #f] ; the initial text for this (set to #f after initialization)
   [mode  'text] ; editing mode: 'scheme or 'text
   [edit?    #f] ; is this text editable?
   [local?   #f] ; is this a local user file? (#f means the server's default)
   [seen?    #f] ; was this text ever seen?
   [curseen? #t] ; was the current version of the text seen?
   [item     #f] ; the toc-list item for this text
   )
  (super-new)
  ;; some accessors
  (define/public (get-frame)   frame)
  (define/public (get-path)    path)
  (define/public (set-path! p) (set! path p))
  (define/public (set-item! i) (set! item i))
  (define/public (is-editable?)
    (and edit? (not (is-a? this interaction-text%))))
  ;; report back when this text was modified
  (define/override (set-modified modified?)
    ;; add to modified list (only after initialization)
    (when (and modified? (not text))
      (set! local? #t)
      (send frame add-to-diff this))
    (super set-modified modified?)
    (refresh-label-color))
  ;; setting the text content, leaving it in a clean state
  (define/public (set-all-text newtext)
    ;; no resetting of text if the file is local, except when initializing
    (when (or text (not local?))
      (set! text #t) ; ensure that we don't report this as a modification
      (define cur (send this get-text))
      (define locked? (send this is-locked?))
      (unless (equal? cur newtext)
        (set! allow-edits #t)
        (send* this
               (begin-edit-sequence)
               (lock #f)
               (erase)
               (insert newtext)
               (set-position 0)
               (clear-undos)
               (lock locked?)
               (set-modified #f)
               (end-edit-sequence))
        (set! allow-edits #f)
        (set! curseen? (eq? this (send frame get-editor)))
        (refresh-label-color))
      (set! text #f)))
  ;; set the color of the label for this text
  (define/public (get-label-color)
    (cond [(send this modified?) unsynced-color]
          [(and edit? (not local?)) unlocal-editable-color]
          [curseen? "black"]
          [else unseen/new-color]))
  (define label-color "black")
  (define/public (refresh-label-color)
    (when item
      (define c (get-label-color))
      (unless (equal? c label-color)
        (define e (and item (send item get-editor)))
        (when e
          (send e change-style (fgcolor->style c) 0 (send e last-position)))
        (set! label-color c))))
  ;; read-only stuff (with an override switch)
  (define allow-edits #f)
  (define/override (blink-caret) (when edit? (super blink-caret)))
  (define/augride (can-insert? start len)
    (and (or allow-edits edit?)
         (or ((+ len (last-position)) . < . max-buffer-size)
             (begin (send frame flashing-message
                          "Buffer too big, insertion disabled")
                    #f))))
  (define/augride (can-delete? start len) (or allow-edits edit?))
  ;; get a read-only indicator by faking the editor:file<%> interface
  (define/public (allow-close-with-no-filename?) #t)
  (define/public (get-can-close-parent) #t)
  (define/public (update-frame-filename) (void))
  (define/public (get-read-write?) edit?)
  ;; selecting this editor in the toc-list
  (define/public (on-select)
    (set! seen? #t)
    (set! curseen? #t)
    (send frame switch-to-editor this)
    (refresh-label-color))
  ;; should an editor be remembered in the editor history?
  (define/public (include-in-history?) #t)
  ;; make the default behavior for control/alt-key do nothing instead of
  ;; inserting the key
  (define/override (on-default-char e)
    (define c (send e get-key-code))
    (when (or (eq? 'release c)
              (not (or (send e get-control-down)
                       (send e get-alt-down)
                       (send e get-meta-down))))
      (super on-default-char e)))
  ;; scroll on space if read-only
  (define/override (on-local-char e)
    (if (and (not edit?) (eq? #\space (send e get-key-code)))
      (send this move-position (if (send e get-shift-down) 'up 'down) #f 'page)
      (super on-local-char e)))
  ;; first line highlighting
  (define keep-first (get-path-option path 'keep-1st-line))
  (when keep-first (send this highlight-first-line #t))
  (define/override (is-special-first-line? line)
    (and keep-first (regexp-match? keep-first line)))
  ;; save/restore cursor and scroll position
  (define saved-positions '())
  (inherit get-position set-position last-position
           get-visible-line-range scroll-to-position
           line-start-position line-end-position)
  (define/public (save-position)
    (define bs (box 0)) (define be (box 0))
    (define bt (box 0)) (define bb (box 0))
    (get-visible-line-range bt bb #f)
    (get-position bs be)
    (define s (unbox bs)) (define e (unbox be))
    (define same? (= s e))
    (define last? (and same? (= s (last-position))))
    (define ls (line-start-position (unbox bt)))
    (define le (line-end-position (unbox bb)))
    (set! saved-positions
          (cons (list (if last? 'last s) (if same? 'same e)
                      ls le (<= ls s e le))
                saved-positions)))
  (define/public (restore-position)
    (when (pair? saved-positions) ; just ignore restore with no save
      (define p (car saved-positions))
      (define (set-pos scroll?)
        (if (eq? 'last (car p))
          (set-position (last-position) 'same #f scroll?)
          (set-position (car p) (cadr p) #f scroll?)))
      (set! saved-positions (cdr saved-positions))
      (set-pos #f)
      (scroll-to-position (caddr p) #f (cadddr p) 'start)
      ;; make the end visible if it was visible
      (when (car (cddddr p)) (set-pos #t))))
  ;; initialization: set text, mode (the default is scheme mode), undo
  (when text (set-all-text text))
  (send this set-surrogate
        (case mode
          [(text) #f]
          [(scheme) (new scheme:text-mode%)]
          [else (error 'tester-text% "bad mode: ~e" mode)]))
  (when edit? (send this set-max-undo-history 'forever))
  (refresh-label-color))

(defclass fixed-text% < tester-text%
  (super-new [seen? #t] [curseen? #t] [mode 'text] [edit? #f])
  (define/override (include-in-history?) #f))

(defclass interaction-text% < tester-text%
  (super-new [mode 'text] [edit? #t] [seen? #t])
  (send this auto-wrap #t)
  (inherit insert)
  (inherit-field frame)
  ;;
  (define outputting   #f) ; are we displaying a line now?
  (define reading      #f) ; are we reading a line now? (if so: a callback)
  (define prompt-point  0) ; where the output ends and the read prompt begins
  (define read-point    0) ; where the prompt ends and the input begins
  (define hidden-input #f) ; are we reading a password? (if so: shadow text%)
  (define reading-enabled #f)
  ;;
  (define-syntax in-input?
    (syntax-rules ()
      [(_) (in-input? (send this get-start-position))]
      [(_ start) (and reading reading-enabled (start . >= . read-point))]))
  ;;
  (define-syntax-rule (allow-output body ...)
    (let ([lock? (send this is-locked?)] [out? outputting])
      (set! outputting #t)
      (send this lock #f)
      body ...
      (send this lock lock?)
      (set! outputting out?)))
  ;; output a line before the reading prompt (if any)
  (define/public (output line [color #f])
    (define str (string-append line "\n"))
    (allow-output
     (insert str prompt-point)
     (when color
       (let ([color (if (eq? color 'message) message-color color)])
         (send this change-style (fgcolor->style color)
               prompt-point (+ prompt-point (string-length str)) #f))))
    (when (and (eq? color 'message) (not (eq? this (send frame get-editor))))
      (set! unread-messages? #t)
      (send this refresh-label-color))
    (set! prompt-point (+ prompt-point (string-length str)))
    (set! read-point   (+ read-point   (string-length str)))
    (send this scroll-to-position prompt-point))
  ;; color indicator for unread messages
  (define unread-messages? #f)
  (define/override (get-label-color)
    (if unread-messages? unread-messages-color "black"))
  (define/override (on-select) (set! unread-messages? #f) (super on-select))
  ;; initiating a line read (note: the callback can be called with #f if
  ;; reading was cancelled)
  (define/public (read-line callback [prompt #f] [password? #f])
    (when reading (error 'interaction-text% "double reading"))
    (when prompt
      (allow-output (insert prompt prompt-point))
      (set! read-point (+ read-point (string-length prompt))))
    (set! hidden-input
          (and password? (let ([t (new text%)])
                           (send t set-admin (send this get-admin))
                           t)))
    (set! reading callback))
  ;; canceling a reader
  (define/public (cancel-reader)
    (when reading
      (define cb reading)
      (set! reading #f)
      (set! hidden-input #f)
      (set! read-point prompt-point)
      (allow-output (send this delete prompt-point (send this last-position)))
      (cb #f)))
  ;; deal with enter
  (define (is-enter? e)
    (define c (send e get-key-code))
    (and (not (eq? 'release c))
         (in-input?)
         (or (memq c '(#\return #\newline))
             (and (send e get-control-down)
                  (memq c '(#\j #\m #\J #\M))))))
  (define/override (on-local-char e)
    (define c (and reading reading-enabled (send e get-key-code)))
    (if (is-enter? e)
      (let ([p  (send this last-position)]
            [cb reading]
            [line (if hidden-input
                    (send hidden-input get-text)
                    (send this get-text read-point))])
        (unless (equal? "" line) ; reject empty lines
          (insert "\n" p)
          (set! reading #f)
          (set! hidden-input #f)
          (set! read-point (add1 p))
          (set! prompt-point (add1 p))
          (send this set-position (add1 p))
          (cb line)))
      (super on-local-char e)))
  ;; password input: a shadow text% holds the text, show bullets, no paste
  (define/override (on-char e)
    (if (and reading reading-enabled hidden-input (not (is-enter? e)))
      (let ([bs (box 0)] [be (box 0)]
            [last-len (send hidden-input last-position)])
        (send* this (begin-edit-sequence) (save-position))
        (send* hidden-input (on-char e) (get-position bs be))
        (send this restore-position)
        (let loop ([delta (- (send hidden-input last-position) last-len)])
          (cond [(negative? delta)
                 (send this delete read-point (- read-point delta))]
                [(positive? delta)
                 (insert (make-string delta (integer->char #x25cf))
                         read-point)]))
        (send* this
               (set-position (+ read-point (unbox bs))
                             (+ read-point (unbox be)))
               (end-edit-sequence)))
      (super on-char e)))
  (define/override (paste [time 0] [start 'start] [end 'same])
    (unless hidden-input (super paste time start end)))
  ;; no setting of all text
  (define/override (set-all-text text) #f)
  ;; prevent editing unless it's on the current line and we're reading
  (define/override (can-insert? start len) (or outputting (in-input? start)))
  (define/override (can-delete? start len) (or outputting (in-input? start)))
  (define/override (blink-caret) (when (in-input?) (super blink-caret)))
  ;; input thread
  (define input-callback void)
  (define input-sema (make-semaphore 0))
  (define input-evt (semaphore-peek-evt input-sema))
  (define/public (enable-input callback)
    (set! input-callback callback)
    (define enable? (and callback #t))
    (unless (equal? enable? reading-enabled)
      (set! reading-enabled enable?)
      ((if enable? semaphore-post semaphore-wait) input-sema)))
  (define (input-loop)
    (sync input-evt)
    (define s (make-semaphore 0))
    (queue-callback
     (λ ()
       (read-line (λ (line)
                    (when line (input-callback line))
                    (semaphore-post s))
                  "> ")))
    (semaphore-wait s)
    (input-loop))
  (thread* input-loop)
  ;; username/password reader
  (define/public (read-username+password callback)
    (define was-enabled? reading-enabled)
    ;; note: playing with reading-enabled is dangerous, since it can
    ;; make `enable-input' block on `input-sema'
    (set! reading-enabled #t)
    (read-line (λ (username)
                 (if username
                   (read-line (λ (password)
                                (set! reading-enabled was-enabled?)
                                (when password (callback username password)))
                              "password: " #t)
                   (set! reading-enabled was-enabled?)))
               "username: "))
  ;; misc
  (define/override (set-modified modified?) (super set-modified #f))
  (define/override (is-special-first-line? line) #f)
  (send this set-max-undo-history 0))

;; ----------------------------------------------------------------------------

(defclass (sanely-navigating-list-mixin %) < %
  (super-new)
  (define-syntax-rule (is-list? i)
    (is-a? i hierarchical-list-compound-item<%>))
  ;; this is based on drracket/private/language-configuration.rkt
  (define cached-fringe #f)
  (define/public (clear-fringe-cache) (set! cached-fringe #f))
  (define/public (get-fringe)
    (unless cached-fringe
      (set! cached-fringe
            ((compose list->vector flatten)
             (let loop ([parent this])
               (map (λ (i) (if (is-list? i) (cons i (loop i)) i))
                    (send parent get-items))))))
    cached-fringe)
  (define/private (select-next inc only-items?)
    (define current (send this get-selected))
    (define (choose item)
      (send this begin-edit-sequence)
      (when current (send current select #f))
      (send item select #t)
      (send item scroll-to) ; a little inconvenient: shows the item+subitems
      (send this end-edit-sequence))
    (define (selectable? item)
      (and (send item get-allow-selection?)
           (not (and only-items? (is-list? item)))
           ;; opened all the way to the top
           (let loop ([p (send item get-parent)])
             (or (not p) (and (send p is-open?) (loop (send p get-parent)))))))
    (define fringe     (get-fringe))
    (define fringe-len (vector-length fringe))
    (define n (and current (for/or ([x (in-vector fringe)] [i (in-naturals 0)])
                             (and (eq? current x) i))))
    ;; need to choose item n, but go on looking for one that is selectable and
    ;; open
    (let loop ([n (if n
                    (min (sub1 fringe-len) (max 0 (inc n)))
                    (modulo (inc fringe-len) (add1 fringe-len)))])
      (when (< -1 n fringe-len)
        (define item (vector-ref fringe n))
        (if (selectable? item) (choose item) (loop (inc n))))))
  (define/override (on-char evt)
    (define code (send evt get-key-code))
    (case code
      [(up)   (select-next sub1 #f)]
      [(down) (select-next add1 #f)]
      ;; right key is fine, but nicer to close after a left
      [(left) (super on-char evt)
              (let ([s (send this get-selected)])
                (when (and s (is-list? s)) (send s close)))]
      [else (super on-char evt)]))
  (define/public (jump-to-prev) (select-next sub1 #t))
  (define/public (jump-to-next) (select-next add1 #t)))

(defclass toc-list% < (sanely-navigating-list-mixin hierarchical-list%)
  (super-new)
  (send this show-focus #t)
  (define (add* kind parent name editor stylespec)
    (define list? (eq? 'list kind))
    (define i (if list? (send parent new-list) (send parent new-item)))
    (send this clear-fringe-cache)
    (send* (send i get-editor)
           (change-style (spec->style stylespec))
           (insert (format (if list? "<~a>" " ~a ") name)))
    (send i user-data editor)
    (when list? (send i open))
    (send* editor (set-item! i) (refresh-label-color))
    i)
  (define tree (make-hash)) ; map reversed paths to editors
  (hash-set! tree '() this)
  (define/public (path->item path)
    (hash-ref tree (reverse path) #f))
  (define/public (add editor)
    (define path  (send editor get-path))
    (define rpath (reverse path))
    (define frame (send editor get-frame))
    (hash-ref!
     tree rpath
     (λ ()
       (when (pair? path)
         (add* 'item
               (let loop ([rp (cdr rpath)])
                 (hash-ref!
                  tree rp
                  (λ ()
                    (add* 'list (loop (cdr rp)) (car rp)
                          (new fixed-text% [path (reverse rp)] [frame frame])
                          toc-item-style))))
               (car rpath) editor (get-path-option path 'style))))))
  (define/public (delete-path path)
    (let loop ([rpath (reverse path)])
      (define i      (hash-ref tree rpath #f))
      (define parent (and i (hash-ref tree (cdr rpath) #f)))
      (when i
        (send (or parent this) delete-item i)
        (hash-remove! tree rpath)
        (when (and parent (null? (send parent get-items)))
          (loop (cdr rpath))))))
  (define/public (delete-editables)
    (for ([i (in-vector (send this get-fringe))])
      (define e (send i user-data))
      (when (send e is-editable?) (delete-path (send e get-path)))))
  ;; hack1: avoid selecting a list when clicked
  (define/override (on-click i)
    (if (is-a? i hierarchical-list-compound-item<%>)
      (begin (send i toggle-open/closed)
             (send i set-allow-selection #f)
             (queue-callback (λ () (send i set-allow-selection #t))))
      (queue-callback
       (λ () (send (send (send i user-data) get-frame) focus-editor)))))
  ;; hack2: make hack1 work
  (send this on-click-always #t)
  ;; hack3: double clicking a list is like two clicks
  (define/override (on-double-select i)
    (when (is-a? i hierarchical-list-compound-item<%>) (send this on-click i)))
  ;; hack results: this is called on lists only when keyboard navigating
  (define/override (on-select i)
    (when i (send (send i user-data) on-select)))
  (define/public (unselect)
    (define i (send this get-selected))
    (define ad? (send this allow-deselect))
    (when i
      (send this allow-deselect #t)
      (send i select #f)
      (send this allow-deselect ad?)))
  (define/override (sort)
    (super sort (λ (i1 i2)
                  (path-list<? (send (send i1 user-data) get-path)
                               (send (send i2 user-data) get-path)))))
  (define/public (begin-edit-sequence)
    (send (send this get-editor) begin-edit-sequence))
  (define/public (end-edit-sequence)
    (send (send this get-editor) end-edit-sequence))
  (define/public (sync-to editor)
    (define i (for/or ([i (in-vector (send this get-fringe))])
                (and (eq? (send i user-data) editor) i)))
    (when i (send this select i))))

(define (update-content content frame toc-list interaction interaction-item
                        #:deleted [deleted '()])
  (define first-call? (null? (send toc-list get-items)))
  (define scroll-pos
    (let ([xb (box 0)] [yb (box 0)] [wb (box 0)] [hb (box 0)])
      (send (send (send toc-list get-editor) get-admin) get-view xb yb wb hb)
      (list (unbox xb) (unbox yb) (unbox wb) (unbox hb))))
  (send toc-list begin-edit-sequence)
  (for ([path (in-list deleted)]) (send toc-list delete-path path))
  (for ([path+text (in-list content)])
    (define path (car path+text))
    (define text (cadr path+text))
    (define mode (get-path-option path 'mode))
    (define existing (send toc-list path->item path))
    (cond
      ;; interaction text
      ;;   link to existing editor, output text only if we see it first
      [(eq? mode 'interaction)
       (unless existing
         (send interaction set-path! path)
         (define item (send toc-list add interaction))
         (set-box! interaction-item item)
         (define sep (make-string 70 #\-))
         (define pos (send interaction get-end-position))
         (send interaction output (string-append sep "\n" text sep))
         (queue-callback
          (λ ()
            (send interaction scroll-to-position pos)
            (sleep/yield 0.25)
            (send interaction scroll-to-position pos))
          #f)
         (send toc-list select item))]
      ;; new file
      [(not existing)
       (send toc-list add
             (new tester-text% [frame frame]
                  [path path] [text text] [mode mode]
                  [edit? (get-path-option path 'editable)]
                  [local? (and (pair? (cddr path+text)) (caddr path+text))]))]
      ;; changed file
      ;;   will ignore redundant updates (due to reconnecting to the server)
      [else (send (send existing user-data) set-all-text text)]))
  (send toc-list sort)
  (send toc-list end-edit-sequence)
  (send toc-list scroll-to (car scroll-pos) (cadr scroll-pos)
        (caddr scroll-pos) (cadddr scroll-pos) #t))

;; ----------------------------------------------------------------------------

(defclass (startable-mixin %) < %
  (define/public (start)
    (dprintf "Initializing kernel services\n")
    (dprintf "Starting GUI\n")
    (send this show #t))
  (super-new))

(defclass (lockable-mixin %) < %
  (field [is-locked? #f])
  (define/public (set-lock-mode mode)
    (let ([mode (and mode #t)])
      (when (and app-locked? (not (eq? mode is-locked?)))
        (set-global-locked-mode! mode)
        (set! is-locked? mode)
        (cond [mode (start-monitor)]
              [monitor-thread (kill-thread monitor-thread)
                              (set! monitor-thread #f)])
        ;; it will lose the focus after unlocking (due to taskbar hacking)
        (SetForegroundWindow this-handle))))
  (define/augment (on-close)
    (send* this (tell-server 'quit) (alert #f "Quitting"))
    (set-lock-mode #f)
    (dprintf "Shutting down GUI...\n")
    (send this quit-on-server-hangup)
    (sleep/yield 3))
  (define/override (start)
    (dprintf "Initializing protection layers\n")
    (set-lock-mode #t)
    (super start))
  ;; no closing
  (define/augment (can-close?) (not is-locked?))
  ;; no moving
  (define/override (on-move x y) (when is-locked? (send this move 0 0)))
  ;; a monitor that tracks when the window is losing focus
  (define this-handle #f)
  (define this-thread-id #f)
  (define monitor-thread #f)
  (define non-windows-alert? (not windows?))
  (define (start-monitor)
    (define (loop)
      (define cur (GetForegroundWindow))
      (cond
        [(equal? this-handle cur) (void)]
        [windows?
         (define title (GetWindowTitle cur))
         (if (equal? (GetWindowThreadProcessId cur) this-thread-id)
           (queue-callback
            (λ ()
              ;; quietly, since this is probably all going to be harmless
              (send this tell-server 'alert
                    (format "Lost focus to: ~a, probably same process"
                            title))))
           (let ([fg? (SetForegroundWindow this-handle)])
             (queue-callback
              (λ ()
                (send this alert "Lost focus to: ~s, ~a" title
                      (if fg? "got it back" "failed to get it back!"))))))]
        [non-windows-alert?
         (set! non-windows-alert? #f)
         (send this tell-server
               'alert "Not a windows machine, no low-level locking")])
      (sleep 1)
      (when is-locked? (loop)))
    (set! this-handle (send this get-handle))
    (set! this-thread-id (GetWindowThreadProcessId this-handle))
    (set! monitor-thread (thread* loop)))
  ;;
  ;; disable unwanted menus
  (define/override (file-menu:create-new?) #f)
  (define/override (file-menu:create-open?) #f)
  (define/override (file-menu:create-open-recent?) #f)
  (define/override (file-menu:create-revert?) #f)
  (define/override (file-menu:create-save?) #f)
  (define/override (file-menu:create-save-as?) #f)
  (define/override (file-menu:create-print?) #f)
  (define/override (file-menu:create-close?) (not app-locked?))
  (define/override (file-menu:create-quit?) (not app-locked?))
  (define/override (file-menu:between-print-and-close menu) (void))
  (define/override (edit-menu:create-preferences?) #f)
  (define/override (edit-menu:between-find-and-preferences menu) (void))
  (define/override (help-menu:create-about?) #f)
  ;;
  (define/public (unlock!)
    (set-lock-mode #f)
    (send this alert "Unlocked"))
  (super-new))

(defclass (communicator-mixin %) < %
  (define/override (start)
    (dprintf "Getting station id\n")
    (set! client-id (get-client-id))
    (set! orig-client-id client-id)
    (set! global-client-id client-id)
    (dprintf "Setting up network connection\n")
    (thread* communicator)
    (super start))
  ;;
  (define client-id #f)
  (define orig-client-id #f)
  (define (get-client-id)
    (define saved (or (get-saved-client-id) ""))
    (if (if (eq? 'if-missing ask-for-client-id?)
          (equal? "" saved)
          ask-for-client-id?)
      (let loop ()
        (define id
          (get-text-from-user "TestOS" "Please enter a station id" this saved))
        (cond [(not id) (dprintf "Aborting...\n") (exit)]
              [(equal? "" id) (loop)]
              [else (unless (equal? saved id) (save-client-id id))
                    (dprintf "Connecting to test server as ~s\n" id)
                    id]))
      saved))
  ;;
  (define-syntax-rule (callback msg . args)
    (queue-callback (λ () (send this msg . args))))
  ;;
  (define teller-thread
    (thread*
     (let ([messages '()])
       (λ ()
         (let loop ()
           (match (thread-receive)
             [(and (cons (or 'message 'alert 'login 'quit) _) msg)
              (set! messages (cons msg messages))
              ;; try to ask the server for a poll, ignore if we can't since a
              ;; poll will eventually be sent anyway
              (with-handlers ([exn? void])
                (define-values [i o] (ssl-connect server-name server-port))
                (define ->server (make-writer o))
                (->server 'tester-client-do-poll)
                (->server client-id)
                (close-input-port i)
                (close-output-port o))]
             [(cons 'get ch)
              (channel-put ch (reverse messages))
              (when (channel-get ch) (set! messages '()))]
             [x (callback alert #f
                          "internal error: bad message to teller-thread: ~e"
                          x)])
           (loop))))))
  (define/public (tell-server . kind+args)
    (thread-send teller-thread kind+args))
  ;;
  (define username #f)
  (define password #f)
  (define need-password? #f) ; set to #f before we're required to set a
                             ; password and also after we've authenticated
  ;;
  (define start-time (current-inexact-milliseconds))
  (define quit-on-hangup #f)
  (define/public (quit-on-server-hangup) (set! quit-on-hangup #t))
  (define (communicator)
    (define close-ports void)
    (with-handlers ([exn?
                     (λ (e)
                       (define msg (exn-message e))
                       (close-ports)
                       (callback alert #f "Disconnected: ~a" msg)
                       (for ([i (in-range (+ 5 (random 20)) -1 -1)])
                         (sleep 1)
                         (callback status* #f "[~a] Disconnected: ~a" i msg))
                       (communicator))])
      ;; note that this doesn't use a certificate, so it will connect to any
      ;; server -- this shouldn't be a problem in this situation, since it
      ;; would be visible that some machine is not connected, and the server is
      ;; up in a very narrow time frame
      (callback status "Connecting...")
      (define-values [i o] (ssl-connect server-name server-port))
      (callback status "Connected to server")
      (set! close-ports (λ () (close-input-port i) (close-output-port o)))
      (define ->server (make-writer o))
      (define server-> (make-reader i read-timeout))
      (define (die! from-server)
        (callback show #f)
        (close-ports)
        (set! quit-on-hangup #t)
        (when from-server (dprintf "~a\n" from-server))
        (send this set-lock-mode #f)
        (exit))
      (define (read-password msg)
        (set! need-password? #t)
        (set! username #f) (set! password #f) ; so we know that we're reading
        (callback read-username+password msg
                  (λ (user pswd)
                    (set! username user)
                    (set! password pswd)
                    (tell-server 'login))))
      (define-syntax-rule (server->callback x ...)
        (let ([s (server->)]) (callback x ... s)))
      (define (loop)
        (define msg (server->))
        (match msg
          ['send-client-id
           ;; always send the original, the server will tell us to change to
           ;; the right one right after this
           (->server orig-client-id)
           (server-> 'ok)]
          [(or 'set-id 'set-id+login)
           (set! client-id (server->))
           (when (eq? msg 'set-id+login)
             (callback status* 'message "Logged in as ~a" client-id)
             (when (and username (not (equal? username client-id)))
               (error "internal error: server sent us a bad username"))
             (set! username client-id)
             (when need-password?
               ;; we were reading a password, cancel it
               (set! need-password? #f)
               (callback cancel-reader)))
           (->server 'ok)]
          [(or 'content 'editable-content)
           (callback status (if (eq? 'editable-content msg)
                              "Getting editable content" "Getting content"))
           (server->callback set-content)
           (->server 'ok)
           (callback status "Ready")]
          ['show-interaction
           (callback switch-to-interaction)
           (->server 'ok)]
          ['show-blank
           (server->callback show-blank)
           (->server 'ok)]
          ['login-required
           (unless (or need-password? username)
             (read-password "Login required"))
           (->server 'ok)]
          ;; doesn't really work -- need to reset line reading, but also a
          ;; bunch of other state like the contents of the interaction window
          ;; ['logout
          ;;  (callback delete-editables)
          ;;  (read-password "Re-login required")
          ;;  (->server 'ok)]
          ['enable-messages
           (server->callback enable-messages)
           (->server 'ok)]
          ['message
           (server->callback flashing-message "message: ~a")
           (->server 'ok)]
          ['freeze
           (server->callback freeze-events)
           (->server 'ok)]
          ['unlock
           (callback unlock!)
           (->server 'ok)]
          ['restart (->server 'ok)
                    (set! restart? #t)
                    (die! "Restarting on server command...")]
          ['die (die! "Shutting down on server command...")]
          ['ping
           (define ping (server->))
           (define time (ping-time ping))
           (define diffs (ping-diffs ping))
           (when time (callback set-clock time))
           (when (pair? diffs) (callback diff-content diffs))
           (->server 'pong)
           (define uptime (- (current-inexact-milliseconds) start-time))
           (define mch (make-channel)) (define dch (make-channel))
           (thread-send teller-thread (cons 'get mch))
           (define messages (channel-get mch))
           (callback get-diffs dch)
           (define diff (channel-get dch))
           (define (ch-reply m d) (channel-put mch m) (channel-put dch d))
           (define reply
             (with-handlers ([exn? (λ (e) (ch-reply #f #f) (raise e))])
               (->server (make-pong uptime messages diff username password))
               (define r (server->))
               (case r
                 [(ok) (ch-reply #t #t)]
                 ;; with a bad password the server shows messages, but
                 ;; won't do our diffs
                 [(bad-password) (ch-reply #t #f)]
                 [else (error "bad server reply to pong message" r)])
               r))
           ;; show a new prompt only if we actually tried a user/pswd
           (when (and username need-password?)
             (if (eq? 'bad-password reply)
               (read-password "Bad password, try again")
               ;; login successful at this point, but no need for a message,
               ;; since we'll be sent a set-id+login message (actually, it will
               ;; turn `need-password?' off, so we'll never get here.)
               (set! need-password? #f)))]
          [(? eof-object?)
           (if quit-on-hangup (die! #f) (error "Lost server connection"))]
          [msg (error (format "Unknown server message: ~e" msg))])
        (loop))
      (->server 'tester-client-connection)
      (server-> 'ok)
      (loop)))
  ;;
  (super-new))

(define help-text
  #<<---help---
The interface of this application has a main view/edit area (where this
text appears), and on the right there is a list of files to flip
through.  Some are editable, which is where you write your answers --
there is no "save" or "submit" button: the text is synchronized to the
server every few seconds.  Colors highlight editable files that you did
not touch, files that were not synchronized yet, and read-only files
that you did not see or that were updated on the server.  Editable files
can be in either scheme mode or text mode.

From time to time you may see messages appearing at the bottom, these
are logged in the startup interactions screen so you don't lose them.
You can also type in text at the prompt of that screen to ask a question
privately.  (Messaging can be disabled: if you cannot type, it's not a
bug.)  You *cannot* run code here, this application is only an editor
for your answers and a viewer for files.

Useful keys:
  F1: toggle this help screen
  F2, F3, F4: cycle the last 2/3/4 files
    (F2 is very useful for flipping between a question text and your
    answer)
  Ctrl-Tab: switch focus between the text and the file list areas
  Ctrl-PageDown, Ctrl-PageUp: jump to next/prev file
  Ctrl-f, Ctrl-g: search (as usual in DrRacket)
  Ctrl-z, Ctrl-y, Ctrl-Z, Ctrl-Y: undo, redo, redo, undo
  Ctrl-+, Ctrl--: change the text size
  Ctrl-s: does nothing -- there is no need to "save" your text

Also: remember that you get a similar editor to the one in DrRacket, it
is useful in a test too!  For example, use the Tab key to indent your
code, and other keys like Alt+( to insert balanced parentheses, etc.

---help---
)

(defclass (keys-mixin %) < %
  (define frozen? #f) ; #t => key/mouse events are blocked
  (define/public (freeze-events b?)
    (set! frozen? b?)
    (send this alert "Application ~a" (if frozen? 'frozen 'unfrozen)))
  (define/override (on-subwindow-event w e) frozen?)
  ;; Password lock release and other control key overrides
  (define password #f)
  (inherit-field is-locked?)
  (define/override (on-subwindow-char w e)
    (define c (send e get-key-code))
    (define (editor) (send this get-editor))
    (cond
      [frozen? #t]
      [password
       (when (char? c)
         (if (eq? c #\return)
           (let ([p (begin0 password (set! password #f))])
             (if (is-master-password? (apply string (reverse p)))
               (send this unlock!)
               (send this alert "Bad Password")))
           (set! password (cons c password))))
       #t]
      [(eq? c 'release) (super on-subwindow-char w e)] ; very common
      [(eq? c 'f1) (send this toggle-help)]
      [(eq? c 'f2) (send this cycle-to 1)]
      [(eq? c 'f3) (send this cycle-to 2)]
      [(eq? c 'f4) (send this cycle-to 3)]
      [(memq c '(wheel-up wheel-down))
       ;; hack: scroll the toc if the mouse is in it
       (or (send this do-wheel-scroll e) (super on-subwindow-char w e))]
      [(send e get-control-down)
       (case c
         [(#\tab) (send this focus-next) #t]
         [(prior) (send this jump-to-prev)]
         [(next)  (send this jump-to-next)]
         [(#\z #\Y) (send (editor) do-edit-operation 'undo) #t]
         [(#\Z #\y) (send (editor) do-edit-operation 'redo) #t]
         [(#\P)
          ;; enter password mode
          (when is-locked? (set! password '()) (send this status* #f "P-mode"))
          #t]
         ;; these are on the menus, but do them here too, so they can be used
         ;; if the application was unlocked (since the menu items will not
         ;; re-appear (actually they do, but be safe))
         [(#\w #\q)
          (when (send this can-close?) (send this on-close))
          #t]
         ;; there is a default C-s for searching, but students tend to use it
         ;; to save, and get confused when it starts searching instead
         [(#\s) #t]
         [else (super on-subwindow-char w e)])]
      [else (super on-subwindow-char w e)]))
  (super-new))

(defclass tester-frame% < ((compose keys-mixin communicator-mixin
                                    lockable-mixin startable-mixin)
                           frame:searchable%)
  ;;
  (define/override (get-entire-label) "Tester")
  ;; use my stuff
  (define/override (get-editor%)
    (define f this) (class interaction-text% (super-new [frame f])))
  (define/override (get-canvas%) canvas:info%)
  ;; readable font & customizations
  (define current-font-size default-font-size)
  (define current-font-bold? #t)
  (define the-font-delta #f)
  (define the-standard-style #f)
  (define (update-style)
    (if (>= current-font-size 0)
      (send the-font-delta set-delta 'change-bigger current-font-size)
      (send the-font-delta set-delta 'change-smaller (- current-font-size)))
    (send the-font-delta set-delta
          'change-weight (if current-font-bold? 'bold 'normal))
    (send the-standard-style set-delta the-font-delta))
  (define (font-size-add +size)
    (set! current-font-size
          (max min-font-size (min max-font-size (+ current-font-size +size))))
    (update-style))
  (define (font-set-bold b?)
    (set! current-font-bold? b?)
    (update-style))
  (define (add-view-menu)
    (define m (new menu% [parent (send this get-menu-bar)] [label "&View"]))
    (new menu-item% [parent m] [label "&Bigger"] [shortcut #\+]
         [callback (λ (m c) (font-size-add +1))])
    (new menu-item% [parent m] [label "&Smaller"] [shortcut #\-]
         [callback (λ (m c) (font-size-add -1))])
    (new checkable-menu-item% [parent m] [label "Bol&d"] [checked #t]
         [callback (λ (m c) (font-set-bold (send m is-checked?)))])
    (set! the-font-delta (make-object style-delta% 'change-family 'modern))
    (set! the-standard-style (send (send (get-editor) get-style-list)
                                   find-named-style "Standard"))
    (update-style))
  ;;
  (define toc-list #f)
  (define/override (make-root-area-container % parent)
    (define s-root
      (super make-root-area-container panel:horizontal-dragable% parent))
    (define r-root (make-object % s-root))
    (set! toc-list (new toc-list% [parent s-root] [stretchable-width #f]))
    (send s-root set-percentages '(5/6 1/6))
    r-root)
  ;;
  ;; focus & navigation
  (define/public (focus-editor) (send (send this get-canvas) focus))
  (define/public (focus-toc) (send toc-list focus))
  (define/public (focus-next)
    (if (send toc-list has-focus?) (focus-editor) (focus-toc)))
  (define (call-preserving-focus thunk)
    (define cur (send this get-focus-window))
    (if cur (begin0 (thunk) (send cur focus)) (thunk)))
  (define/public (jump-to-prev) (send toc-list jump-to-prev))
  (define/public (jump-to-next) (send toc-list jump-to-next))
  (define editor-history '())
  (define (add-to-history editor)
    (when (send editor include-in-history?)
      (set! editor-history (cons editor (remq editor editor-history)))))
  (define/public (cycle-to n)
    (when (< n (length editor-history))
      (define editor (list-ref editor-history n))
      (send toc-list sync-to editor)
      (switch-to-editor editor)))
  ;; hack: scroll the toc if the mouse is in it
  (define/public (do-wheel-scroll e)
    (let ([x  (send e get-x)]
          [tx (send toc-list get-x)])
      (and (<= tx x) (< x (+ tx (send toc-list get-width)))
           (let ([y  (send e get-y)]
                 [ty (send toc-list get-y)])
             (and (<= ty y) (< y (+ ty (send toc-list get-height)))
                  (begin (send toc-list on-char e) #t))))))
  ;;
  ;; text content
  (define current-editor #f)
  (define interaction-item (box #f))
  (define/override (get-editor) (or current-editor (super get-editor)))
  (define/public (switch-to-interaction)
    (define item (unbox interaction-item))
    (if item
      (send toc-list select item)
      ;; we didn't see it, so nothing to leave selected in the toc-list
      (begin (send toc-list unselect)
             (switch-to-editor interaction-editor))))
  ;; convenience to make editors that can be switched to
  (define-syntax-rule (make-switcher make-editor-expr)
    (let ([editor #f] [pre-item #f] [pre-editor #f])
      (define (switch-to)
        (define prev-editor (get-editor))
        (unless (eq? prev-editor editor)
          (set! pre-editor prev-editor)
          (set! pre-item (send toc-list get-selected))
          (send toc-list unselect)
          (switch-to-editor editor)))
      (define (switch-back)
        (when (and (eq? (get-editor) editor) pre-editor)
          (when pre-item (send toc-list select pre-item))
          (switch-to-editor pre-editor)
          (set! pre-editor #f)
          (set! pre-item #f)))
      (define (toggle)
        (if (eq? (get-editor) editor) (switch-back) (switch-to)))
      (λ (m)
        (unless editor (set! editor make-editor-expr))
        (case m
          [(toggle) (toggle)]
          [(show) (switch-to)]
          [(hide) (switch-back)]))))
  (define help-switcher
    (make-switcher (new fixed-text% [text help-text] [frame this])))
  (define/public (toggle-help) (help-switcher 'toggle))
  (define blank-switcher
    (make-switcher (new fixed-text% [path '()] [frame this])))
  (define/public (show-blank blank?) (blank-switcher (if blank? 'show 'hide)))
  (define/public (switch-to-editor editor)
    (define prev-editor (get-editor))
    (define canvas (send this get-canvas))
    (send* prev-editor (save-position) (lock #t)) ; save pos, stop coloring
    (send canvas set-editor editor)
    (send* editor (lock #f) (restore-position)) ; restore pos, allow coloring
    (set! current-editor editor)
    (add-to-history editor)
    ;; need to switch focus for update-info to work
    (call-preserving-focus (λ () (focus-editor) (send this update-info))))
  (define/public (delete-editables)
    (send toc-list delete-editables))
  ;; getting content from the server
  (define (update-content* content [deleted '()])
    (update-content content this toc-list interaction-editor interaction-item
                    #:deleted deleted))
  (define/public (set-content content) (update-content* content))
  (define/public (diff-content diff)
    (define-values [deleted new]
      (partition (λ (x) (eq? 'deleted (car x))) diff))
    ;; treat added and changed files the same, `update-content' will notice
    ;; no-changes anyway
    (update-content* (map cdr new) (map cadr deleted)))
  ;; sending content to the server
  (define modified-editors '())
  (define/public (add-to-diff e)
    (unless (memq e modified-editors)
      (set! modified-editors (cons e modified-editors))))
  (define/public (get-diffs ch)
    (channel-put ch (for/list ([e modified-editors])
                      (list (send e get-path) (send e get-text))))
    (when (channel-get ch)
      (for ([e modified-editors]) (send e set-modified #f))
      (set! modified-editors '())))
  ;;
  ;; add a status and a clock message area, log status messages
  (define status-message #f)
  (define clock-message  #f)
  (define (make-status-messages)
    (define info (send this get-info-panel))
    (define msg (new message% [parent info] [label "Initializing..."]
                     [font (spec->font message-font)]
                     [stretchable-width #t]))
    (define clk (and show-clock? (new message% [parent info] [label "??:?? "]
                                      [font (spec->font clock-font)])))
    (send info change-children
          (λ (l)
            (let* ([l (remq* (list msg clk) l)]
                   [l (if clk (cons clk l) l)])
              (cons msg l))))
    (set! status-message msg)
    (set! clock-message clk))
  (define/public (set-clock str)
    (when clock-message (send clock-message set-label str)))
  ;; interaction and messages
  (define interaction-editor #f)
  (define/public (enable-messages b?)
    (send interaction-editor enable-input
          (and b? (λ (line) (send this tell-server 'message line)))))
  (define/public (cancel-reader) (send interaction-editor cancel-reader))
  (define/public (read-username+password message callback)
    (send* interaction-editor (output message alert-color)
                              (read-username+password callback)))
  (define status-off-timer
    (new timer% [notify-callback
                 (λ () (send status-message set-label ""))]))
  (define/public (status* color fmt . args)
    (send status-off-timer stop)
    (send status-off-timer start 5000 #t)
    (let* ([str (apply format fmt args)]
           [str (if ((string-length str) . > . 200)
                  (substring str 0 200) str)])
      (send status-message set-label str)
      (when color (send interaction-editor output str color))))
  (define/public (status fmt . args)
    (status* status-color fmt . args))
  (define/public (alert fmt/type . args)
    (let* ([type? (not (string? fmt/type))]
           [type  (if type? fmt/type 'alert)]
           [fmt   (if type? (car args) fmt/type)]
           [args  (if type? (cdr args) args)]
           [msg   (apply format fmt args)])
      (when type (send this tell-server type msg))
      (fprintf (current-error-port) "alert: ~a\n" msg)
      (send interaction-editor output msg alert-color)
      (status* #f "*** ~a ***" msg)))
  (define/public (flashing-message fmt . args)
    (define str (apply format fmt args))
    (define str*
      (if ((string-length str) . > . 200) (substring str 0 200) str))
    (define (show s)
      (queue-callback (λ () (send status-message set-label s))))
    (status* 'message "~a" str)
    (unless (eq? interaction-editor (get-editor))
      (thread (λ ()
                (define hi 0.1)
                (for ([i (in-range 0.0 hi 0.01)])
                  (sleep i) (show "") (sleep (- hi i)) (show str*))))))
  ;;
  (define/override (start)
    (set! interaction-editor (get-editor))
    (add-view-menu)
    (make-status-messages)
    (frame:remove-empty-menus this)
    (super start)
    (send the-clipboard set-clipboard-string "" 0) ; clear initial clipboard
    (SetForegroundWindow (send this get-handle))) ; grab the focus
  (if app-locked?
    (let-values ([(left top) (get-display-left-top-inset)]
                 [(width height) (get-display-size #t)])
      (super-new
       ;; use the whole screen, no decorations
       [x (- left)] [y (- top)] [width width] [height height]
       [style `(;; 'float is inconvenient after unlocking, and it's not
                ;; needed with the protection we get from WinLock
                no-caption no-resize-border
                hide-menu-bar no-system-menu)]))
    (begin (super-new) (queue-callback (λ () (send this maximize #t))))))

;; ----------------------------------------------------------------------------

(application:current-app-name "Tester") ; we don't actually use this

(dprintf "TestOS booting\n")
(define restart? #t)
(let ([exit (exit-handler)])
  (let loop ()
    (kill-threads)
    (when restart?
      (set! restart? #f)
      (parameterize ([exit-handler
                      (λ xs (if restart? (loop) (apply exit xs)))])
        (reset-preferences)
        (set-global-locked-mode! #f)
        (send (new tester-frame%) start)
        (yield 'wait)
        (loop)))))
(exit) ; the framework should do it, so this is just in case
