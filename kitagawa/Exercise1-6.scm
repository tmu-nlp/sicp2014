#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;;実行の仕方
;;gosh Exercise1-6.scm

(print "Exercise1-6:")

(define (square x) (* x x))

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
  guess
   (sqrt-iter (improve guess x)
    x)))

(define (average x y)
 (/ (+ x y) 2))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
 (average guess (/ x guess)))

(define (sqrt x)
 (sqrt-iter 1.0 x))

(print (sqrt 9))

(define (new-if predicate then-clause else-clause)
 (cond (predicate then-clause)
  (else else-clause)))

(define (sqrt-iter guess x)
 (new-if (good-enough? guess x)
  guess
   (sqrt-iter (improve guess x)
    x)))

(print "ifは特殊形式なのでgood-enough?の結果によってどちらかを選択するが、new-ifを使っいる方はcondを使っておりcondの中の条件式をどちらも実行する。ここで計算する必要のないsqrt-iterを計算しようとしループに入ってしまう。")
(print (sqrt 9))
