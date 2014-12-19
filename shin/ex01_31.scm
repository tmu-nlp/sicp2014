#!/usr/bin/gosh

(define (square x) (* x x))
(define (next i) (+ i 1))
;;----------------------------------------------------------
;;再帰
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

;;反復
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (term i) i)
  (product term 1 next n))

(define (wallis-pi n)
  (define (term i)
    (/ (* (* 2.0 i) (* 2.0 (+ i 1.0)))
       (square (+ (* 2.0 i) 1.0))))
  (* 4 (product term 1 next n)))

(define product product-rec)
;(define product product-iter)
#?=(factorial 6)
#?=(factorial 100)
#?=(wallis-pi 10)
#?=(wallis-pi 100)
#?=(wallis-pi 1000)