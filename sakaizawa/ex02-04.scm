#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-04.scm

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define z (cons 3 4))
(print "(define z (cons 3 4))")
(print )
(print "(car z)")
(print (car z))
(print )
(print "(cdr z)")
(print (cdr z))
(print )


