#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-22.scm

(define (square x) (* x x))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes idx max)
  (cond ((even? idx) (search-for-primes (+ idx 1) max))
        ((> idx max) #f) 
        (else
          (begin
            (timed-prime-test idx)
            (search-for-primes (+ idx 2) max)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(search-for-primes 1000 1020)
(print "") 
(search-for-primes 10000 10040)
(print "") 
(search-for-primes 1000000 1000050)
(print "") 
(search-for-primes 10000000 10000100)
(print "")

