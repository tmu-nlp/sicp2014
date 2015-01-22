#!/usr/bin/gosh
; -*- coding:utf-8 -*-

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (pair z value result)
  (if (= (remainder z value) 0)
      (pair (/ z value) value (+ result 1))
      result))

(define (my-car z) (pair z 2 0))
(define (my-cdr z) (pair z 3 0))

(define z (my-cons 6 4))
#?=(my-car z)
#?=(my-cdr z)
