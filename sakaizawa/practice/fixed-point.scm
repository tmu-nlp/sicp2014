#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh fixed-point.scm

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(print "(fixed-point cos 1.0)")
(print (fixed-point cos 1.0))

(print "(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)")
(print (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))


