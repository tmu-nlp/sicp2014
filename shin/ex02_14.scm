#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
(load "./ex02_12.scm")
;----------------------------------
;; make-interval
;(define (make-interval a b) (cons a b))
;(define (upper-bound x) (cdr x))
;(define (lower-bound x) (car x))
;
;; make-center-width, center, width
;(define (make-center-width c w)
;  (make-interval (- c w) (+ c w)))
;(define (center i)
;  (/ (+ (lower-bound i) (upper-bound i)) 2))
;(define (width i)
;  (/ (- (upper-bound i) (lower-bound i)) 2))
;
;; make-center-percent, persent
;(define (make-center-persent c p)
;  (make-interval (- c (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))
;(define (percent i)
;  (* (/ (width i) (center i)) 100.0))
;------------------------------------------

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(define r (make-center-percent 100 1))
#?=(percent r)
#?=r
#?=(percent (div-interval r r))

