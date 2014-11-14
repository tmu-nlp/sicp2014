#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multi a b)
  (cond ((= b 0) 0)
      ((even? b) (multi (double a) (halve b)))
      (else (+ a (multi a (- b 1))))))

(multi 7 8)
;; => 56
