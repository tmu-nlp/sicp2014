#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;;実行の仕方
;;gosh Exercise1-3.scm


;Define a procedure that takes three numbers as arguments 
;and returns the sum of the squares of the two larger numbers.

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (Ex03 x y z)
    (cond ((and (<= x y ) (<= x z) ) (sum-of-squares y z))
        ((and (<= y x) (<= y z) ) (sum-of-squares x z))
        ((and (<= z x ) (<= z y) ) (sum-of-squares x y))
        )
)

(print (Ex03 1 2 3))
(print (Ex03 3 1 2))
(print (Ex03 2 3 1))
(print (Ex03 1 2 2))
