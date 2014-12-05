#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh 1-29.scm

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
         (define h (/ (- b a) n))
         (define (yk k) (f (+ a (* h k))))
         (define (simpson-term k) (* (cond ((or (= k 0) (= k n)) 1) ((odd? k) 4) (else 2)) (yk k)))
(* (/ h 3) (sum simpson-term 0 inc n)))

(print "(simpson-integral cube 0 1 100)")
(print (simpson-integral cube 0 1 100))

(print "(simpson-integral cube 0 1 1000)")
(print (simpson-integral cube 0 1 1000))