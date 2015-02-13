#lang racket
(define (adjoin-set x set)
  (cond ((null? set)
         (cons x set))
        ((< x (car set))
         (cons x set))
        (else
         (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 3 '(2 4 8))