#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-10-10


(define (newif predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (newif (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)
  )
)

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
