#lang racket

(define (reverse items)
  (if (null? items)
      null
      (append (reverse (cdr items)) (cons (car items) null))))

(define (deep-reverse items)
  (cond ((null? items) null)
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items)) (cons (deep-reverse (car items)) null)))))

(define x (list (list 1 2) (list 3 4)))

x
'((1 2) (3 4))

(reverse x)
'((3 4) (1 2))

(deep-reverse x)
'((4 3) (2 1))

(deep-reverse (list (list 66 1 2) (list 55 3 4)))
'((4 3 55) (2 1 66))