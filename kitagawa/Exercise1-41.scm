#!/usr/bin/gosh
;;execution gosh Exercise1-41.scm

(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(print "(((double (double double)) inc ) 5)")
(print (((double (double double)) inc ) 5))

