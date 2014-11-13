#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-38.scm

;;自然対数 e = 2.718281828459045235

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (euler-number k)
  (if (= (remainder (+ k 1) 3) 0)
      (* 2 (/ (+ k 1) 3))
      1))

(print "(+ 2 (cont-frac (lambda (x) 1.0) euler-number 50))")
(print (+ 2 (cont-frac (lambda (x) 1.0) euler-number 50)))



