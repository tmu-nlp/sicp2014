#!/usr/bin/gosh
(use srfi-19)
(load "./ex01_21.scm")
;---------
;(define (square x) (* x x))
;
;(define (divides? a b)
;  (= (remainder b a) 0))
;
;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))
;
;(define (smallest-divisor n)
;  (find-divisor n 2))
;
;(define (prime? n)
;  (= n (smallest-divisor n)))
;---------

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
  (cond ((= n 0) (newline) 'done)
        ((even? from) (search-for-primes (+ from 1) n))
        ((timed-prime-test from) (search-for-primes (+ from 2) (- n 1)))
        (else (search-for-primes (+ from 2) n))))

(define (search n)
  (display (search-for-primes n 3))
  (newline))

#?=(search 1000)     ;; => 
#?=(search 10000)    ;; => 
#?=(search 100000)   ;; => 
#?=(search 1000000)  ;; => 
;#?=(search 0)
;(search 13)
;(search-for-primes 13 3)
;
  
