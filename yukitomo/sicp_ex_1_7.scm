;sicp_1_7.scm
(define (sqrt-iter2 guess pre-guess x)
    (if (good-enough? guess pre-guess)
        guess
        (sqrt-iter2 (improve guess x) guess x)))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))
;(guess - pre-guess)/guess < 0.001 ならばよい
(define (good-enough? guess pre-guess)
    (< (/ (abs (- guess pre-guess)) guess) 0.001))

(define (sqrt x)
    (sqrt-iter2 1.0 100.0 x))