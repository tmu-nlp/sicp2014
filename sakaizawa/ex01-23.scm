#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-23.scm

(use srfi-19)
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next n)
  (if (= n 2)
        (+ n 1)
        (+ n 2)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1009)
(print "") 
(timed-prime-test 1013)
(print "") 
(timed-prime-test 1019)
(print "") 

(timed-prime-test 10007)
(print "") 
(timed-prime-test 10009)
(print "") 
(timed-prime-test 10037)
(print "") 


