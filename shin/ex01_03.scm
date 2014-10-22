#!/usr/bin/gosh

(define (square x) (* x x)) 
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ex01_03 a b c)
    (cond ((and (< a b) (< a c)) (sum-of-squares b c)) ;aが最小
          ((and (< b a) (< b c)) (sum-of-squares a c)) ;bが最小
          (else (sum-of-squares a b)))) ;cが最小

(print (ex01_03 1 2 3))
(print (ex01_03 2 1 3))
(print (ex01_03 2 3 1))
