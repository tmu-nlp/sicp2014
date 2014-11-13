#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-37.scm

;;再帰的手続き
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(print "(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)")
(print (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11))

;;反復的手続き
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

