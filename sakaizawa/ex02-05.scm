#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-05.scm

(define (ex-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (pair z value result)
  (if (= (remainder z value) 0)
      (pair (/ z value) value (+ result 1))
      result))
(define (ex-car z)
  (pair z 2 0))
(define (ex-cdr z)
  (pair z 3 0))

(define z (ex-cons 3 2))
(print "(define z (ex-cons 3 2))")
(print z)
(print )
(print "(ex-car z)")
(print (ex-car z))
(print )
(print "(ex-cdr z)")
(print (ex-cdr z))
(print )

