#lang racket

(require (for-syntax syntax/parse))

; Add syntax for definition for multiple variables in a single expression. 
(define-syntax (define* stx)
  (syntax-parse stx
    [(_define* (~seq x:id e:expr) ...)
     (syntax/loc stx
       (begin
         (define x e)
         ...))]))

(define* RPL_WELCOME "001"

         ERR_UNKNOWNCOMMAND "421"
         ERR_ERRONEUSNICKNAME "432"
         ERR_NEEDMOREPARAMS "461")

; Parses an IRC message into ...
;
; According to RFC1459 Section 4.2.3.2 "Clients should not use prefix when
; sending a message from themselves; if they use a prefix, the only valid prefix
; is the registered nickname associated with the client.".
; Therefore the prefix can be ignored on client messages.
(define (parse-msg msg)
  ; Split the message on space, except after a <space><colon>.
  (let* ([x (string-split msg #rx" +:")]
         [y (append (string-split (car x) " ") (cdr x))])
         (if (string-prefix? (car y) ":") (rest y) y)))

; 
(define (msg-to-str msg)
  (string-join msg " " #:before-last
               (if (string-contains? (last msg) " ") " :" " ")))

(define (PONG) "Not implemented.")

(define (USER out msg)
  (let ([params (rest msg)])
    ; TODO     
    ))

(define (NICK) "Not implemented.")

(define (JOIN) "Not implemented.")

(define (PRIVMSG) "Not implemented.")

(provide parse-msg
         msg-to-str)
