#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-11.scm

(define (f n)
  if (< n 3) n
     (+ (f (- n 1) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
  (if (< n 3)
        n
        (f2-iter 2 1 0 (- n 2))))

(define (f2-iter a b c count)
  (if (= count 0) 
        a
        (f2-iter (+ c (* 2 b) (* 3 a) a b) (- count 1)))




