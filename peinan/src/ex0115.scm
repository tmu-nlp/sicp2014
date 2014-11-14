#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (square x) (* x x))
(define (even? x) (= 0 (remainder x 2)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
      (else (* b (fast-expt b (- n 1))))))

(define (fast-expt2 a b n)
  (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (fast-expt (* a b) b (- n 1)))))

#?=(fast-expt 2 10)
#?=(fast-expt2 1 2 10)
