#lang racket/base

(require rackunit
         "irc.rkt")

(check-equal? (parse-msg "CMD") '("CMD"))

(check-equal? (parse-msg "CMD P1 P2") '("CMD" "P1" "P2"))

(check-equal? (parse-msg "CMD P1 P2 :P3 P33 P333")
              '("CMD" "P1" "P2" "P3 P33 P333"))

(check-equal? (parse-msg ":PREFIX CMD P1 P2 :P3 P33 P333")
              '("CMD" "P1" "P2" "P3 P33 P333"))
