#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-29.scm

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define next inc)
  (define (coef x)
    (cond ((or (= x 0) (= x n)) 1)
          ((even? x) 2)
          (else 4)))
  (define (term k) (* (coef k) (f (+ a (* k h)))))
  (* (/ h 3) (sum term 0 next n)))

(print "(simpson-integral cube 0 1 100)")
(print (simpson-integral cube 0 1 100))

(print "(simpson-integral cube 0 1 1000)")
(print (simpson-integral cube 0 1 1000))

