#!/usr/bin/gosh
; -*- coding:utf-8 -*-
;;----------------------------------------------------------
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline)
  )
;;----------------------------------------------------------

(define (make-segment start end) (cons start end))
(define (start-segment p) (car p))
(define (end-segment p) (cdr p))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (avg a b) (/ (+ a b) 2))

(define (midpoint-segment seg)
  (let ((sp (start-segment seg))
        (ep (end-segment seg)))
    (make-point (avg (x-point sp) (x-point ep))
                (avg (y-point sp) (y-point ep)) )))

(define p1 (make-point 1.0 0.0))
(define p2 (make-point 0.0 1.0))
(define seg1 (make-segment p1 p2))
(define p3 (midpoint-segment seg1))

;#?=(print-point p3)