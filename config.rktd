([workdir "."] ; can be relative to the directory where it is started
 [server-name "something.somewhere.edu"]
 [server-port 8888]
 [client-port #f] ; makes it possible to pipe through a different port
 [master-password #"????????????????????????????????"]
 [server
  [poll-frequency 5] ; poll/update clients approximately every N seconds
  [read-timeout 45]  ; timeout (in seconds) for reading from a client
  [read-limit 300]   ; limit reading a client message to Nkb
  [accept-ips #f]    ; can be a prefix of the ip like "127.0.0"; can be a list
  [logfile "LOG"]    ; log file, or #f for none
  [stderr #t]        ; use standard error or not (in addition to a logfile)
  [prompt "> "]      ; prompt for server controller, #f => no prompt
  ;; login mode can be #f (no login, work saved based on client id or ip), or
  ;; #t (login required)
  [login-mode #t]
  ;; types of password verification, and extra arguments, currently only
  ;; (md5sum <password-file>) is supported
  [password-verifier (md5sum "passwords.rktd")]
  ;; subdirectories of workdir where content files are found, and client files
  ;; are saved (backup directories are client-dir with a timestamp)
  [content-dir "content"]
  [clients-dir "clients"]
  [allow-messages? #t] ; allow clients to send messages by default?
  ;; backup approximately this many seconds, can be #f to disable backups,
  ;; backups are when polling so the most this can get (eg, with 0) is a backup
  ;; on every poll
  [backup-interval 60]
  ;; command and flags to create a backup directory, #f means use racket code
  [backup-command ("cp" "-al")]
  ]
 [netboot
  ;; the netboot file to create -- it can be a racket file or a batch file
  ;; which will create such a racket file and run it.  (Determined by the
  ;; suffix of this name.)
  [file "netboot.rkt"]
  ;; when the netboot file runs, it will contact the server to get the client
  ;; source files -- these will be put in the following directory
  [client-dir "tester"]
  ;; the first file is the one that should be executed
  [client-files ("tester.rkt" "shared.rkt" "WinLockDll.dll")]
  ;; the options below apply only for a batch file
  [racket-path "C:\\Program Files\\Racket"] ; path/s of Racket dir/s to use
  [batch-prefix '()] ; a list of batch lines to put at the beginning
  [batch-loop? #t] ; if this is true, then the batch file will call the racket
                   ; file in a loop, which can be convenient to restart it (it
                   ; will wait for a keypress between runs)
  ]
 [client
  ;; whether the client application locks the machine in "kiosk mode", this
  ;; will most likely need to be overridden in the local configuration
  [app-locked? #f]
  ;; timeout (in seconds) for reading from the server (should be relatively
  ;; large, since the client uses this timeout when waiting for a poll)
  [read-timeout 90]
  ;; if this is #f, then the client id should be there, or the default will be
  ;; its ip address; otherwise it can be #t or `if-missing'
  [ask-for-client-id? if-missing]
  ;; store the client-id in a local path (avoid the system temp directory,
  ;; which might not be local); can be () to avoid saving it
  [id-file-directories ("c:/temp" "c:/tmp" "/tmp")]
  ;; should the client show a clock?  (controlled by the server, so the content
  ;; can be changed dynamically)
  [show-clock? #t]
  ;; limit buffer sizes to Nkb, should be much smaller than `read-limit' above
  [buffer-limit 200]
  ;; GUI customizations
  [default-font-size 6]
  [min-font-size -8]
  [max-font-size +16] ; not big so others can't see easily
  [message-font    (16 decorative normal bold)]
  [clock-font      (16 modern normal bold)]
  [toc-item-style  (14 modern normal bold (240 240 192))]
  [status-color  (128 128 128)]
  [alert-color   (255   0   0)]
  [message-color (192   0 192)]
  [unsynced-color         (255 0 0)]
  [unlocal-editable-color (0 192 0)]
  [unseen/new-color       (192 0 192)]
  [unread-messages-color  (192 0 0)]
  ]
 ;; this is a useful template, should be further customized for each test
 [path-specs
  (;; ignore hidden files
   [(* #rx"^[.]" *)
    ignore #t]
   [("messages") ; need a file with some contents for this
    order -10
    mode interaction
    style (14 modern normal bold (224 255 224))]
   [("test.txt")
    order -2
    style (14 modern normal bold (255 224 255))]
   [("answers" *)
    order -1
    editable #t
    style (14 modern normal bold (255 255 224))]
   [("test" *)
    comparator qa-comparator] ; eg: ... Question3b, Answer3b, ...
   [("lectures" *)
    keep-1st-line #rx"^20[0-9][0-9]-[0-9][0-9]-[0-9][0-9] "]
   [(* #rx"[.]rkt$")
    order +1
    mode scheme
    keep-1st-line #rx"^ *#lang "]
   ;; default for all files
   [(*)
    mode text
    editable #f
    style (12 modern normal normal #f)])])

;; <size>     ::= <integer>
;; <family>   ::= base | default | decorative | roman | script | swiss
;;              | modern | symbol | system
;; <style>    ::= base | normal | italic | slant
;; <weight>   ::= base | normal | bold | light
;; <color>    ::= <string> | (<integer> <integer> <integer>)
;; <font>     ::= (<size> <family> <style> <weight>)
;; <style>    ::= (<#f|size> <#f|family> <#f|style> <#f|weight> <#f|bgcolor>)
;; <ignore>   ::= <boolean>           ; default=#f
;; <mode>     ::= text | scheme | interaction
;; <editable> ::= <boolean>
;; <order>    ::= <integer>           ; default=0, use to bring entries up/down
;; <keep1st>  ::= <regexp>            ; determines if 1st line is always shown
;; <pathitem> ::= <string> | <regexp> | * | ?
;; <path>     ::= (<pathitem> ...)    ; (*) means default
;; <pathdata> ::= ignore <ignore> | order <order>
;;              | mode <mode> | editable <editable>
;;              | style <style> | keep-1st-line <keep1st>
;; <pathspecs> ::= (<path> <pathdata> ...)
;;    ; there should at most one path with a mode of interaction, its contents
;;    ; is displayed on the client's window initially; if there are none,
;;    ; students will not be able to switch back to the interaction window!
