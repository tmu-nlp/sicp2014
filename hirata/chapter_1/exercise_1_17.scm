#!/usr/local/bin/gosh

(define (double x)
        (* x 2))
(define (halve y)
        (/ y 2))
(define (even? z)
        (= (remainder z 2) 0))

(define (fast_mul a b)
  (cond ((= b 0) 0)
	((even? b) (fast_mul (double a) (halve b)))
    (else (+ (fast_mul (double a) (halve (- b 1))) a)))
  )

(print (fast_mul 3 5))
