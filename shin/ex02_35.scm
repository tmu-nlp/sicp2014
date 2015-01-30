#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
(load "./ex02_33.scm")

;2-2-2
;-----------
;(define (count-leaves x)
;  (cond ((null? x) 0)
;        ((not (pair? x)) 1)
;        (else (+ (count-leaves (car x))
;                 (count-leaves (cdr x))))))
;-----------
;-----------
;(define (accumulate op initial sequence)
;  (if (null? sequence)
;      initial
;      (op (car sequence)
;          (accumulate op initial (cdr sequence)))))
;-----------

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (cond ((null? x) 0)
                                         ((not (pair? x)) 1)
                                         (else (count-leaves x)))) t)))

(define x (cons (list 1 2) (list 3 4 (list 5 6 7))))
#?=x
#?=(count-leaves x)

