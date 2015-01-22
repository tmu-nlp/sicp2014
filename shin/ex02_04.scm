#!/usr/bin/gosh
; -*- coding:utf-8 -*-

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define foo (my-cons 1 2))
#?=(my-car foo)
#?=(my-cdr foo)