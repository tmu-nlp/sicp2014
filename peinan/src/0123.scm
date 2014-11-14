#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(use ex0119)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next test-divisor)
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

(define (timed-prime-test n)
  (newline)
    (display n)
      (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (and (prime? n)
    (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
    (display elapsed-time))

(define (search-for-primes from n)
  (cond ((= n 0) (newline))
    ((even? from) (search-for-primes (+ from 1) n))
        ((timed-prime-test from) (search-for-primes (+ from 2) (- n 1)))
  (else (search-for-primes (+ from 2) n))))

(define (search n)
  (search-for-primes n 3))

(search 1000)     ;; => *** #<time-duration 0.000015000>
(search 10000)    ;; => *** #<time-duration 0.000035000>
(search 100000)   ;; => *** #<time-duration 0.000209000>
(search 1000000)  ;; => *** #<time-duration 0.000410000>
