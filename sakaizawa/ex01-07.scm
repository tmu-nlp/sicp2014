#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-07.scm

(define (square x) (* x x)) 

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2)) 

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(print "(sqrt 0.00000004)")
(print (sqrt 0.00000004))
(print )

(print "(sqrt 4000000000000000000000000000000000000)")
(print (sqrt 4000000000000000000000000000000000000))
(print )

(print "上のままだとうまく行かないのでgood-enough?を書き換えるとうまくいく")
(define (good-enough? guess x)
  (< (abs (/ (- (square guess) x) (* 2 x))) 0.001))


(print "(sqrt 0.00000004)")
(print (sqrt 0.00000004))
(print )

(print "(sqrt 4000000000000000000000000000000000000)")
(print (sqrt 4000000000000000000000000000000000000))
(print )
