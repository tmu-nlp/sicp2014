#lang scheme
(define (square x) (* x x))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) n)
     ((divides? test-divisor n) test-divisor)
     (else
      (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (zero? (modulo b a)))
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

;run
(prime? 101)