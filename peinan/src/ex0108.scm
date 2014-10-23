#!/usr/bin/env gosh
;; coding: utf-8

(define (sq x) (* x x))

(define (good-enough? guess previous-guess)
  (< (abs
       (- 1
          (/ guess previous-guess)))
     0.001)
)

(define (cuber-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
    guess
    (cuber-iter (improve guess x)
                guess
                x)
  )
)

(define (improve guess x)
  (/ (+ (/ x
           (sq guess))
        (* 2 guess))
     3)
)

(define (good-enough? guess previous-guess)
  (< (abs (- 1
             (/ guess previous-guess)))
     0.001)
)

(define (cuber x)
  (cuber-iter 1.0 10.0 x))
