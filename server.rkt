; TODO: Decide order of (... sender msg) arguments and stay consistent.

#lang racket

(require racket/tcp
         racket/date
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
  SERVER-VERSION "1.0.0a"
  PORT (or (getenv "PORT") 6667)
  PASS "secret"
  STARTED_AT (seconds->date (current-seconds)))

(define*
  RPL_WELCOME "001"
  RPL_YOURHOST "002"
  RPL_CREATED "003"
  RPL_MYINFO "004"
  RPL_NOTOPIC "331"
  RPL_TOPIC "332"
  RPL_VERSION "351"
  RPL_INFO "371"

  ERR_UNKNOWNCOMMAND "421"
  ERR_ERRONEUSNICKNAME "432"
  ERR_NEEDMOREPARAMS "461")

; A channel represents an IRC channel and
(define channel%
  (class object%
    (init init-name)
    (super-new)
    (field [users (list)])
    (field [topic ""])))

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
(define users (list))

; TODO: Replace this with proper logging.
(define (log-debug str)
  (displayln str))

; Starts the server and spawns a thread to handle incoming
; connections.
; Returns a function to stop the server.
(define (start)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen PORT 5 #t "0.0.0.0"))
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
    (set! users (append users (list user)))

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
  (let ([cmd (string-upcase (first msg))])
    (case cmd
      [("NICK") (NICK sender msg)]
      [("USER") (USER sender msg)]
      [("JOIN") (JOIN sender msg)]
      [("PART") (PART sender msg)]
      [("VERSION") (VERSION sender msg)]
      [("TOPIC") (TOPIC sender msg)]
      [("INFO") (INFO sender msg)]
      [("PRIVMSG") (PRIVMSG sender msg)]
      [("PING") (PING sender msg)])))

(define (broadcast-msg-to-chan msg channel-name #:exclude-users [excludes '()])
  (let* ([chan (hash-ref channels channel-name)]
         [usrs (get-field users chan)])
    (for-each (λ (cu)
                 (when (not (member (get-field nick cu) excludes))
                   (send-msg msg cu)))
              usrs)))

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

; Converts a list to a valid IRC message string. The last parameter is prefixed
; with a <colon>, if it contains spaces or begins with a <colon>. Latter is
; required to support text that begins with a <colon>, like smileys.
(define (msg-to-str msg)
  (string-join msg " "
               #:before-first ":"
               #:before-last (if (or (string-contains? (last msg) " ")
                                     (string-prefix? (last msg) ":"))
                               " :" " ")))

(define (PING sender msg)
  (send-msg (list "PONG" (second msg) "localhost") sender))

(define (PONG) "Not implemented.")

(define (NICK sender msg)
  (define-values (nick) (second msg))

  ; [...] unique nickname having a maximum length of nine (9) characters.
  ; TODO: Check nick against grammar and uniqueness.
  (cond
    [(> (string-length nick) 9)
     (send-msg (list ERR_ERRONEUSNICKNAME nick ":Erroneous nickname") sender)]

    [else (set-field! nick sender nick)]))

(define (USER sender msg)
  (match-define (list user host server name) (rest msg))
  (send-msg (list "localhost" RPL_WELCOME "ctas" "Welcome to the Racket IRC server ctas!ctas@localhost.") sender)
  (send-msg (list "localhost" RPL_YOURHOST "ctas" (format "Your host is Hermit@~a." SERVER-VERSION)) sender)
  (send-msg (list "localhost" RPL_CREATED "ctas" "This server was created once upon a time.") sender)
  (send-msg (list "localhost" RPL_MYINFO "ctas"  "-") sender))

(define (JOIN sender msg)
   (let* ([chans (string-split (second msg) ",")]
          [keys  (if (= (length msg) 3) (string-split (third  msg)) #f)])

     (for-each (λ (chan-name)
                  ; Create channel, if it does not exist.
                  (when (not (hash-has-key? channels chan-name))
                    (hash-set! channels chan-name (new channel% [init-name chan-name])))

                  (let ([target-chan (hash-ref channels chan-name)])
                      ; Join channel.
                      (set-field! users target-chan (append (get-field users target-chan) (list sender)))

                      ; Broadcast channel join to all users in channel.
                      (broadcast-msg-to-chan
                        (list (get-field nick sender) "join" chan-name)
                        chan-name)

                      (TOPIC sender (list "TOPIC" chan-name))

                      )) chans)))


     ; RPL_TOPIC
     ; RPL_NAMEREPLY

(define (PART sender msg)
   (let ([channel-names (string-split (second msg) ",")])
     (for-each (λ (channel-name)
                  (let* ([channel       (hash-ref channels channel-name)]
                         [channel-users (get-field users channel)])
                    (set-field! users channel (remove sender channel-users eq?))
                    (broadcast-msg-to-chan (list (get-field nick sender) "PART") channel-name)))
               channel-names)))

(define (PRIVMSG sender msg)
  (let ([sender-nick  (get-field nick sender)]
        [channel-name (second msg)]
        [text         (third  msg)])
    (broadcast-msg-to-chan
      (list sender-nick "PRIVMSG" channel-name text)
      channel-name #:exclude-users (list sender-nick))))


(define (INFO sender msg)
  ; TODO: Validate the server matches ours.
  (send-msg (list
              "localhost"
              RPL_INFO
              (get-field nick sender)
              (format "Hermit@~a (https://github.com/cihantas/hermit) - Started ~a"
                      SERVER-VERSION
                      (date->string STARTED_AT))
              ) sender))

(define (VERSION sender msg)
  ; TODO: Validate the server matches ours.
  ; TODO: fix space char workaround for prefix
  (send-msg (list "localhost" RPL_VERSION (get-field nick sender)
                  (format "Hermit@~a" SERVER-VERSION) "localhost" "") sender))

(define (TOPIC sender msg)
  ; TODO Check for permissions.
  (let* ([channel-name (second msg)]
         [channel (hash-ref channels channel-name)]
         [channel-topic (get-field topic channel)])
    (cond
      ; TODO If user is not on channel reply with ERR_NOTONCHANNEL.

      ; If a topic is given, update it on the channel.
      [(>= (length msg) 3)
       (set-field! topic channel (third msg))
       (send-msg (list (get-field nick sender) "TOPIC" channel-name (third msg)) sender)]

      ; No topic parameter. Channels topic is set. Reply with RPL_TOPIC.
      [(non-empty-string? channel-topic)
       (send-msg (list "localhost" RPL_TOPIC (get-field nick sender) channel-name channel-topic) sender)]

      ; No topic parameter. Channels topic is not set. Reply with RPL_NOTOPIC.
      [else
       (send-msg (list "localhost" RPL_NOTOPIC (get-field nick sender) channel-name "No topic is set.") sender)])))

(provide parse-msg
         msg-to-str)

; Start the server. Wait for a SIGINT or SIGTERM to stop it and exit the
; program.
(define stop (start))
