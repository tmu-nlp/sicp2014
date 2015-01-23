#!/usr/bin/gosh

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define torelance 0.00001)

;;----------------------------------------------------------

(define (iterative-improve test improve)
  (lambda (guess)
    (if (test guess)
        guess
        ((iterative-improve test improve) (improve guess)))))

(define (sqrt x)
  (define (test guess)
    (< (abs (- (square guess) x)) torelance))
  ((iterative-improve test (average-damp (lambda (y) (/ x y)))) 1.0))

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))