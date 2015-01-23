#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
(load "./ex02_36.scm")
;---------------------------------------
;(define (accumulate op initial sequence)
;  (if (null? sequence)
;      initial
;      (op (car sequence)
;          (accumulate op initial (cdr sequence)))))
;
;(define (accumulate-n op init seqs)
;  (if (null? (car seqs))
;      ()
;      (cons (accumulate op init (map car seqs))
;            (accumulate-n op init (map cdr seqs)))))
;-----------------------------------------

;;内積
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;;matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

;;転置
(define (transpose mat)
  (accumulate-n cons () mat))

;;matrix-*-matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
       (map (lambda (x) (matrix-*-vector cols x)) m)))

(define v (list 1 2))
(define w (list 3 4))
(define m (list (list 1 2) (list 3 4)))
#?=v
#?=w
#?=m

#?=(dot-product v w)
#?=(matrix-*-vector m v)
#?=(transpose m)
#?=(matrix-*-matrix m m)