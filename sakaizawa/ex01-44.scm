#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-44.scm

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (smooth f)
  (define dx 0.1)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(print "((n-fold-smooth square 2) 5)")
(print ((n-fold-smooth square 2) 5))

