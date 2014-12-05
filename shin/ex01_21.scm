#!/usr/bin/gosh



(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))


;#?=(smallest-divisor 199)    ;; => 199
;#?=(smallest-divisor 1999)   ;; => 1999
;#?=(smallest-divisor 19999)  ;; => 7
;#?=(prime? 19999)

;(smallest-divisor 13)
;(find-divisor 13 2)
;(find-divisor 13 3)
;(find-divisor 13 4)
;16>13 より 13