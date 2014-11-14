#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-31.scm

(define (inc x) (+ x 1))

(define (identify x) x)

(define (square x) (* x x))

;;反復的手続き
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;;再帰的手続き
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))


(print "(product identify 1 inc 5)")
(print (product identify 1 inc 5))

(print "(product-rec identify 1 inc 5)")
(print (product-rec identify 1 inc 5))

;;n!
(define (factorial n)
  (define (term x) x)
  (product term 1 inc n))

(print "(factorial 6)")
(print (factorial 6))

(define (pi-wallis times)
  (define (term n)
    (/ (* (* 2.0 n) (* 2.0 (+ n 1.0)))
       (square (+ (* 2.0 n) 1.0))))
  (* 4 (product term 1 inc times)))

(print "(pi-wallis 1000)")
(print (pi-wallis 1000))



