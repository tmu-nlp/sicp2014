#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


(define (make-segment vect1 vect2)
	(cons vect1 vect2))

(define (start-segment segment)
	(car segment))

(define (end-segment segment)
	(cdr segment))


