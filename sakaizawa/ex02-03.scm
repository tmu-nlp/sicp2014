#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-03.scm

(define (square x) (* x x))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;縦・横を表す２つの線分で表す
(define (make-rectangle a b) (cons a b))
(define (height-rectangle a) (length-segment (car a)))
(define (width-rectangle a) (length-segment (cdr a)))
(define (length-segment a)
  (sqrt (+ (square (- (x-point (start-segment a))
                      (x-point (end-segment a))))
           (square (- (y-point (start-segment a))
                      (y-point (end-segment a)))))))

(define rec1 (make-rectangle 3 4))
(print "(define rec1 (make-rectangle 3 4))")

;幅×高さの長さで表す
(define (make-rectangle height width) (cons height width))
(define (height-rectangle a) (car a))
(define (width-rectangle a) (cdr a))

(define rec2 (make-rectangle 4 6))
(print "(define rec2 (make-rectangle 4 6))")

;周囲・面積計算
(define (perimeter r)
  (+ (* 2 (height-rectangle r))
     (* 2 (width-rectangle r))))
(define (area r)
  (* (height-rectangle r) (width-rectangle r)))

(print "(perimeter rec1)")
(print (perimeter rec1))
(print )
(print "(area rec1)")
(print (area rec1))
(print )
(print "(perimeter rec2)")
(print (perimeter rec2))
(print )
(print "(area rec2)")
(print (area rec2))
(print )
