#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(print (gcd 28 16))
;; => 4
