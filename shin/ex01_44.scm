#!/usr/bin/gosh
(load "./ex01_43.scm")
;;---------------------------------------------
;(define (square x) (* x x))
;(define (compose f g)
;  (lambda (x) (f (g x))));
;
;(define (repeated f n)
;  (if (= n 0)
;      (lambda (x) x)
;      (compose f (repeated f (- n 1)))))
;;---------------------------------------------

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;#?=((n-fold-smooth square 2) 5)