#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (A x y)
  (cond ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
  (else (A (- x 1)
    (A x (- y 1))))))

#?=(A 1 10)
#?=(A 2 4)
#?=(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

#?=(f 3)
#?=(g 10)
#?=(h 4)
