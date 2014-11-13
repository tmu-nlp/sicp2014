#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-32.scm

(define (identify x) x)

(define (square x) (* x x))

(define (inc x) (+ x 1))

;;反復的手続き
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(print "(sum idenify 1 inc 10)")
(print (sum identify 1 inc 10))

(print "(product identify 1 inc 5)")
(print (product identify 1 inc 5))

;;再帰的手続き
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-rec combiner null-value
                         term (next a) next b))))

