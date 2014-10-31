#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-19.scm

(define (even? n) (= (remainder n 2) 0)) 

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))      ; p'を計算
                   (+ (square q) (* 2 p q))       ; q'を計算
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(print "(fib 5)")
(print (fib 5))

(print "(fib 8)")
(print (fib 8))


