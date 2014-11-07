#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-18.scm

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n) (= (remainder n 2) 0))

(define (multiply x y) (itr x y 0))

(define (itr x y z)
  (cond ((= x 0) z)
        ((even? x) (itr (halve x) (double y) z))
        (else (itr (- x 1) y (+ z y)))))

(define (mul a b) (multiply a b))


(print "(mul 3 4)")
(print (mul 3 4))

(print "(mul 2 -4)")
(print (mul 2 -4))


