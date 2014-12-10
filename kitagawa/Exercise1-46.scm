#!/usr/bin/gosh
;;execution gosh Exercise1-46.scm

(define tolerance 0.00001)

(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))


(define (iterative-improve enough? imp)
  (lambda (guess)
    (define (iter x)
      (if (enough? x) x (iter (imp x))))
    (iter guess)))

(define (sqrt-itr x)
  (define (good-enough? guess)
    (< (abs (- x (square guess))) tolerance))
  (define (improve guess) (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point-itr f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (next guess))) tolerance))
  (define (next guess) (f guess))
  ((iterative-improve close-enough? next) first-guess))

(print "(sqrt-itr 4)")
(print (sqrt-itr 4))

(print "(fixed-point-itr cos 1.0)")
(print (fixed-point-itr cos 1.0))



