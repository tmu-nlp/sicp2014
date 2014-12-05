#!/usr/bin/gosh

(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (double f)
  (lambda (x) (f (f x))))

#?=(((double (double double)) inc) 5)
