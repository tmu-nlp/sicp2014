#!/usr/local/bin/gosh

(define (double x)
        (* x 2))
(define (halve y)
        (/ y 2))

(define (multi-iter n a b)
  (cond ((= b 0) n)
        ((even? b) (multi-iter n (double a) (halve b)))
        (else (multi-iter (+ n a) a (- b 1)))))

(print (multi-iter 0 4 5))
