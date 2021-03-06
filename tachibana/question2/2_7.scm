#!/usr/bin/env gosh
; -*- coding: utf-8; -*-

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(print "(add-interval (cons 10 100)(cons 10 200))")
(print (add-interval (cons 10 100)(cons 10 200)))
