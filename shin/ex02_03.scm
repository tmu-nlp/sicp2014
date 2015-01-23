#!/usr/bin/gosh
; -*- coding:utf-8 -*-
(load "./ex02_02.scm")
;;----------------------------------------------------------
;(define (make-segment start end) (cons start end))
;(define (start-segment p) (car p))
;(define (end-segment p) (cdr p))
;
;(define (make-point x y) (cons x y))
;(define (x-point p) (car p))
;(define (y-point p) (cdr p))
;;----------------------------------------------------------
(define (square x) (* x x))

;縦横の線分
;(define (make-rectangle a b) (cons a b))
;(define (height-rectangle a) (length-segment (car a)))
;(define (width-rectangle a) (length-segment (cdr a)))
;(define (length-segment a)
;  (sqrt (+ (square (- (x-point (start-segment a))
;                      (x-point (end-segment a))))
;           (square (- (y-point (start-segment a))
;                      (y-point (end-segment a)))))))

;幅×高さの長さ
(define (make-rectangle height width) (cons height width))
(define (height-rectangle a) (car a))
(define (width-rectangle a) (cdr a))

;周囲・面積計算
(define (perimeter r)
  (+ (* 2 (height-rectangle r))
     (* 2 (width-rectangle r))))
(define (area r)
  (* (height-rectangle r) (width-rectangle r)))

#?=(define rec1 (make-rectangle 3 4))
#?=(perimeter rec1)
#?=(area rec1)
