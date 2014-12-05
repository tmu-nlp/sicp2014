#!/usr/bin/gosh

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (euler-number k)
  (if (= (remainder (+ k 1) 3) 0)
      (* 2 (/ (+ k 1) 3))
      1))

#?=(+ 2 (cont-frac (lambda (x) 1.0) euler-number 50))
(print "e =    2.7182818284590452")