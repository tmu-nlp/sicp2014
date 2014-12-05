#!/usr/bin/gosh
(use srfi-19)
(load "./ex01_22.scm")


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))));nがNEXTに

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(print "ex23--------")
#?=(search 1000)     ;; => *** #<time-duration 0.000015000>
#?=(search 10000)    ;; => *** #<time-duration 0.000035000>
#?=(search 100000)   ;; => *** #<time-duration 0.000209000>
#?=(search 1000000)  ;; => *** #<time-duration 0.000410000>

