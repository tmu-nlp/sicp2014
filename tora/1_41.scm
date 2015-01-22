#lang racket
(define (double f) (lambda (x) (f (f x))))
(define (inc x) (+ 1 x))
(double )
(((double (double double)) inc )1)