#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-33.scm

(define (identify x) x)

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (filtered-accumulate filter
                             combiner null-value
                             term a next b)
  (define (target? x)
    (if (filter x) (term x) null-value))
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (target? a) result))))
  (iter a null-value))

(define (sum-square-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (mutual-prime-product n)
  (define (f x) (= 1 (gcd x n)))
  (filtered-accumulate f * 1 identify 1 inc (- n 1)))





