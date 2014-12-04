#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-45.scm

(define tolerance 0.00001)

(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (n-root x n)
  (define (pow x n)
    (if (= n 0) 1 (* x (pow x (- n 1)))))
  (let ((k (floor (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp k)
                  (lambda (y) (/ x (pow y (- n 1)))))
                 1.0)))

(print "(n-root 8 3)")
(print (n-root 8 3))

