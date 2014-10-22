#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex-08.scm

(define (cub x) (* x x x)) 
(define (square x) (* x x)) 

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average (* 2 guess) (/ x (square guess))))

(define (average x y)
  (/ (+ x y) 3)) 

(define (good-enough? guess x)
  (< (abs (- (cub guess) x)) 0.001))

(define (cube x)
  (cube-iter 1.0 x))

(print "(cube 27)")
(print (cube 27))
(print )

(print "(cube 64)")
(print (cube 64))
(print )
