#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-16.scm

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-expt-ex b n) (fast-iter b n 1))

(define (fast-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-iter (square b) (/ n 2) a))
        (else (fast-iter b (- n 1) (* a b)))))

(print "(fast-expt-ex 2 4)")
(print (fast-expt-ex 2 4))

(print "(fast-expt-ex 2 5)")
(print (fast-expt-ex 2 5))

(print "(fast-expt-ex 3 4)")
(print (fast-expt-ex 3 4))







