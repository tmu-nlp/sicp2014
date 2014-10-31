#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;;実行の仕方
;;gosh Exercise1-8.scm

(print "Exercise1-8:")

(define (square x) (* x x))

(define (cube x)
  (* x x x))

(define (cubic-iter guess x)
 (if (good-enough? guess x)
  guess
   (cubic-iter (improve guess x)
    x)))

(define (good-enough? guess x)
 (< (abs (- (cube guess) x)) 0.001))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3)) ;;ここを問題文の式に修正

(define (cubic x)
 (cubic-iter 1.0 x))

(print "8の立方根")
(print (cubic 8))
(print "1000000の立方根")
(print (cubic 1000000))
(print "0.000001の立方根")
(print (cubic 0.000001))

(print "\ngood-enough?を修正すると...\n")

(define (good-enough? guess x)
 (< (abs (/ (- (cube guess) x) (* 3 (square guess)))) 0.001))
(print "8の立方根")
(print (cubic 8))
(print "1000000の立方根")
(print (cubic 1000000))
(print "0.000001の立方根")
(print (cubic 0.000001))


