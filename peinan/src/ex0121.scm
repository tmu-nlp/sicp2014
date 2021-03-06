#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)    ;; => 199
(smallest-divisor 1999)   ;; => 1999
(smallest-divisor 19999)  ;; => 7
