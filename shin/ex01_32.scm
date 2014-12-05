#!/usr/bin/gosh
(load "./ex01_31.scm")


(define (square x) (* x x))
(define (next i) (inc i))
(define (inc x) (+ x 1))
(define (cube x) (* x x x))
;;----------------------------------------------------------
;(define (factorial n)
;  (define (term i) i)
;  (product term 1 next n))

;(define (wallis-pi n)
;  (define (term i)
;    (/ (* (* 2 i) (* 2 (+ i 1)))
;       (square (+ (* 2 i) 1))))
;  (* 4 (product term 1 next n)))
;;----------------------------------------------------------

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;再帰プロセス
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

;;反復プロセス
(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner null-value (term a)) term (next a) next b)))

#?=(define accumulate accumulate-rec)
;(define accumulate accumulate-iter)
#?=(factorial 10)
#?=(factorial 100)
#?=(wallis-pi 10)
#?=(wallis-pi 100)
#?=(wallis-pi 1000)
#?=(sum cube 1 inc 10)