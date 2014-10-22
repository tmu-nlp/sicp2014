#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-03.scm


(define (square x) (* x x)) 
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ex03 a b c)
  (cond ((and (>= a b) (>= b c)) (sum-of-squares a b)) 
        ((and (>= a c) (>= c b)) (sum-of-squares a c))

        ((and (>= b a) (>= a c)) (sum-of-squares b a)) 
        ((and (>= b c) (>= c a)) (sum-of-squares b c))

        ((and (>= c a) (>= a b)) (sum-of-squares c a))
        ((and (>= c b) (>= b a)) (sum-of-squares c b))))


(print "(ex03 1 2 3)")
(print (ex03 1 2 3))
(print )

(print "(ex03 1 3 2)")
(print (ex03 1 3 2))
(print )

(print "(ex03 2 1 3)")
(print (ex03 2 1 3))
(print )

(print "(ex03 2 3 1)")
(print (ex03 2 3 1))
(print )

(print "(ex03 3 1 2)")
(print (ex03 3 1 2))
(print )

(print "(ex03 3 2 1)")
(print (ex03 3 2 1))
(print )

