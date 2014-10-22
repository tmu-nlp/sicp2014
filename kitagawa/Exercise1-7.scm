#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;;実行の仕方
;;gosh Exercise1-7.scm

(print "Exercise1-7:")

(define (square x) (* x x))

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
  guess
   (sqrt-iter (improve guess x)
    x)))

(define (average x y)
 (/ (+ x y) 2))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
 (average guess (/ x guess)))

(define (sqrt x)
 (sqrt-iter 1.0 x))

(print "2の平方根")
(print (sqrt 2))
(print "9の平方根")
(print (sqrt 9))
(print "10000000000000000000000000000の平方根")
(print (sqrt 100000000000000000000000000000000000))
(print "0.000001の平方根")
(print (sqrt 0.000001))

(print "\ngood-enough?を変えると...\n")

(define (good-enough? guess x)
  (< (abs (/ (- (square guess) x) (* 2 x))) 0.001))

(print "2の平方根")
(print (sqrt 2))
(print "9の平方根")
(print (sqrt 9))
(print "1000000000000000000000000の平方根")
(print (sqrt 1000000000000000000000000000))
(print "0.000001の平方根")
(print (sqrt 0.000001))


