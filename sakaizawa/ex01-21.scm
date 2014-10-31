#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-21.scm

(define (square x) (* x x))


(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
  (= (remainder b a) 0))


(print "(smallest-divisor 199)")
(print (smallest-divisor 199))

(print "(smallest-divisor 1999)")
(print (smallest-divisor 1999))

(print "(smallest-divisor 19999)")
(print (smallest-divisor 19999))

