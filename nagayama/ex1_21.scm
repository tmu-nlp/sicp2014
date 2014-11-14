#lang scheme

; 前提関数
(define (square x) (* x x))

; 約数探査
(define (smallest-divisior n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

;run
(smallest-divisior   199) ; ans =  199
(smallest-divisior  1999) ; ans = 1999
(smallest-divisior 19999) ; ans =    7