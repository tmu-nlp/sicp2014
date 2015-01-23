#!/usr/bin/gosh
; -*- coding:utf-8 -*-

(define (inc x) (+ x 1))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) 
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f) 
    (lambda (x) 
      ((m f) ((n f) x)))))

; x = 0, f = x+1
(define (f x) (+ 1 x))
#?=((zero f) 0)
#?=((one f) 0)
#?=((two f) 0)
#?=(((add-1 two) f) 0)
#?=(((add two two) f) 0)

