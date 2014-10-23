#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-10-10

;code/problem-1-7-2.scm

(define (sqrt-iter2 guess pre-guess x)
  (if (good-enough? guess pre-guess)
    guess
    (sqrt-iter2 (improve guess x) guess x)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess pre-guess)
  (< (/ (abs (- guess pre-guess)) guess) 0.001))

(define (sqrt x)
  (sqrt-iter2 1.0 100.0 x))
