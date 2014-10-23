#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-10-10

(define (sq x) (* x x))

(define (sum-sq x y)
  (+ (sq x)
     (sq y)
  )
)

(define (sum-sq-big2 x y z)
  (cond
    ((and (< x y) (< x z)) (sum-sq y z))
    ((and (< y z) (< y x)) (sum-sq x z))
  )
)
