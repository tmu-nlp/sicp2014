#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-20.scm

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(gcd 206 40)
  (gcd 40 6)
  (gcd 6 4)
  (gcd 4 2)
  (gcd 2 0)
;;2

;;remainder演算は4回実行される
;;作用的順序の場合、remainder演算は10回？



(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

