#!/usr/bin/gosh

(define (square x) (* x x))
(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        ;(and (print guess)
             (sqrt-iter (improve guess x) x)));)

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(print (sqrt 2))

;--------------------------------------小さいほうはこれで解決

(define (good-enough? guess x)
    (< (/ (abs (- (square guess) x)) 
          (* 2 x))
       0.001))

(print (sqrt 0.000002))

;-------------------------------------

(define (sqrt-iter2 guess pre-guess x)
    (if (good-enough? guess pre-guess)
        guess
        (sqrt-iter2 (improve guess x) guess x)))


(define (good-enough? guess pre-guess)
    (< (/ (abs (- guess pre-guess)) guess) 0.001))

(define (sqrt_3 x)
    (sqrt-iter2 1.0 100.0 x))

(print (sqrt_3 2))
(print (sqrt_3 0.000002))
(print (sqrt_3 200000000000000000000000000000))
