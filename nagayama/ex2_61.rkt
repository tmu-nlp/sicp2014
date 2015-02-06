#lang racket

; adjoin-set : 順序付けされたリストへの挿入

(define (adjoin-set x set)
  (cond ((null? set)
         (cons x '()))
        ((= x (car set))
         (cons x set))
        ((< x (car set))
         (cons x set))
        (else
         (cons (car set) (adjoin-set x (cdr set))))))


; run
(adjoin-set 6 '(2 3 5 7 11))
(adjoin-set 6 '())


