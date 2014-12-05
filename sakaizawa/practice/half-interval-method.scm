#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh half-interval-method.scm

(define (square x) (* x x)) 

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (positive? x) (> x 0))

(define (negative? x) (< x 0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(print "(half-interval-method sin 2.0 4.0)")
(print (half-interval-method sin 2.0 4.0))

(print "(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)")
(print (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0))

(print "(half-interval-method (lambda (x) (- (* x x )  4)) 1.0 5.0)")
(print (half-interval-method (lambda (x) (- (* x x )  4)) 1.0 5.0))

