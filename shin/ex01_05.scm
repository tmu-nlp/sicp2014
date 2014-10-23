#!/usr/bin/gosh

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0   
      y)) 
(print (test 0 p))
(pirnt (test 0 (p)))
