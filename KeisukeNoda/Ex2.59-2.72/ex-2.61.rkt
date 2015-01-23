#lang racket

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= (car set) x) (cons (car set) (adjoin-set x (cdr set))))
        (else (cons (car set) (cons x (cdr set))))))

(adjoin-set 5 '(1 2 4 6 7)) 
"=>" 
'(1 2 4 5 6 7)

(adjoin-set 50 '(1 2 4 6 7)) 
"=>" 
'(1 2 4 6 7 50)