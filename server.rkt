#lang racket

(require racket/tcp
         (for-syntax syntax/parse))

; Add syntax for definition of multiple variables in a single expression.
(define-syntax (define* stx)
  (syntax-parse stx
    [(_define* (~seq x:id e:expr) ...)
     (syntax/loc stx
       (begin
         (define x e)
         ...))]))

(define*
  SERVER-NAME "Racket"
  VERSION "1.0.0a"
  PORT (or (getenv "PORT") 6667)
  PASS "secret")

(define*
  RPL_WELCOME "001"
  RPL_YOURHOST "002"
  RPL_CREATED "003"
  RPL_MYINFO "004"

  ERR_UNKNOWNCOMMAND "421"
  ERR_ERRONEUSNICKNAME "432"
  ERR_NEEDMOREPARAMS "461")

; A channel represents an IRC channel and

(define channel%
  (class object%
    (super-new)
    (init init-name)
    (field [users (list)])))

; A user encapsul

(define user%
  (class object%
    (init output-port)
    (super-new)
    (field [op output-port]
           [nick "*"]
           [username #f]
           [realname #f])))


(define channels (make-hash))
(define users '())

; TODO: Replace this with proper logging.
(define (log-debug str)
  (displayln str))

; Starts the server and spawns a thread to handle incoming
; connections.
; Returns a function to stop the server.
(define (start)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen PORT 5 #t))
    (log-debug (format "Listening on ~a." PORT))

    ; Handle incoming connections on a new thread.
    (define (loop)
      (handle-connection listener)
      (loop))
    (thread loop))

  ; Return a function to stop the server.
  ; TODO Stop the server on SIGINT, SIGTERM.
  (λ ()
    (custodian-shutdown-all main-cust)))

(define (handle-connection listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])

    ; Accept blocks until a connection can be accepted.
    (define-values (in out) (tcp-accept listener))
    (log-debug "Client connected.")

    ; Instantiate a user for the connected client.
    (define user (new user% [output-port out]))
    ; TODO: Maybe only add user after USER command.
    (append users user)

    (define (loop)
      (let* ([msg-str (read-line in 'return-linefeed)]
             [msg (parse-msg msg-str)])
        (log-debug (string-append "< " msg-str))
        (dispatch-msg msg user))


      #|
      THIS IS NOT WORKING, BUT NEEDED, FIX!
      (let ([msgs (string-split (read-string 512 in) "\r\n")])
        (displayln msgs)
        (for-each (λ (msg)
                    (dispatch-msg msg out)) msgs))
|#
      (loop))
    (thread loop)))

(define (dispatch-msg msg sender)
  (let ([cmd (first msg)])
    (case cmd
      [("NICK") (NICK sender msg)]
      [("USER") (USER sender msg)]
      [("PING") (PING sender msg)])))

(define (send-msg msg sender)
  (let ([rpl (string-append (msg-to-str msg) "\r\n")])
    (display (string-append "> " rpl))
    (display rpl (get-field op sender))
    (flush-output (get-field op sender))))

; Parses an IRC message into ...
;
; :PREFIX CMD P1 P2 :P3 P33 P333
;
; According to RFC1459 Section 4.2.3.2 "Clients should not use prefix when
; sending a message from themselves; if they use a prefix, the only valid prefix
; is the registered nickname associated with the client.". Therefore the prefix
; can be ignored on client messages.
(define (parse-msg msg)
  ; Split the message on space, except after a <space><colon>.
  (let* ([x (string-split msg #rx" +:")]
         [y (append (string-split (car x) " ") (cdr x))])
    (if (string-prefix? (car y) ":") (rest y) y)))

;
(define (msg-to-str msg)
  (string-join msg " " #:before-last
               (if (string-contains? (last msg) " ") " :" " ")))

(define (PING sender msg)
  (send-msg (list "PONG" (second msg) "localhost") sender))

(define (PONG) "Not implemented.")

(define (NICK sender msg)
  (define-values (nick) (second msg))

  ; [...] unique nickname having a maximum length of nine (9) characters.
  ; TODO: Check nick against grammar and uniqueness.
  (cond
    [(> (string-length nick) 9)
     (send-msg (list ERR_ERRONEUSNICKNAME nick ":Erroneous nickname") sender)]))

(define (USER sender msg)
  (match-define (list user host server name) (rest msg))
  (send-msg (list ":localhost" RPL_WELCOME "ctas" "Welcome to the Racket IRC server ctas!ctas@localhost.") sender)
  (send-msg (list ":localhost" RPL_YOURHOST "ctas" (format "Your host is Hermit@~a." VERSION)) sender)
  (send-msg (list ":localhost" RPL_CREATED "ctas" "This server was created once upon a time.") sender)
  (send-msg (list ":localhost" RPL_MYINFO "ctas"  "-") sender))


(define (PART) 'not-implemented)

(define (PRIVMSG) 'not-implemented)

(provide parse-msg
         msg-to-str)

; Start the server. Wait for a SIGINT or SIGTERM to stop it and exit the
; program.
(define stop (start))
