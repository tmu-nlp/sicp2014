#!/usr/bin/gosh

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

#?=(sum cube 1 inc 10)

;(define (inc x) (+ x 1))
;(define (sum-integers a b) (sum identity a inc b))
;(define (sum-integers 1 10)