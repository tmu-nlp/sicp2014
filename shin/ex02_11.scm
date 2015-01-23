#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
(load "./ex02_07.scm")

;-------------------------------------------------
;(define (add-interval x y)
;  (make-interval (+ (lower-bound x) (lower-bound y))
;                 (+ (upper-bound x) (upper-bound y))))
;
;(define (mul-interval x y)
;  (let ((p1 (* (lower-bound x) (lower-bound y)))
;        (p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y)))
;        (p4 (* (upper-bound x) (upper-bound y))))
;    (make-interval (min p1 p2 p3 p4)
;                   (max p1 p2 p3 p4))))
;
;(define (div-interval x y)
;  (mul-interval x 
;                (make-interval (/ 1.0 (upper-bound y))
;                               (/ 1.0 (lower-bound y)))))
;
;(define (make-interval a b) (cons a b))
;(define (lower-bound x) (car x))
;(define (upper-bound x) (cdr x))
;
;#?=(add-interval r1 r2)
;#?=(mul-interval r1 r2)
;---------------------------------------------------

(define (new-mul-interval x y)
  (let ((ux (upper-bound x))
        (lx (lower-bound x))
        (uy (upper-bound y))
        (ly (lower-bound y)))
    (cond [(and (> ux 0) (> lx 0))
           (cond [(and (> uy 0) (> ly 0))
                  (make-interval (* ux uy) (* lx ly))]
                 [(and (> uy 0) (< ly 0))
                  (make-interval (* ux uy) (* lx ly))]
                 [(and (< uy 0) (< ly 0))
                  (make-interval (* ux ly) (* lx uy))])]
          [(and (> ux 0) (< lx 0))
           (cond [(and (> uy 0) (> ly 0))
                  (make-interval (* ux uy) (* lx ly))]
                 [(and (> uy 0) (< ly 0))
                  (make-interval (min (* ux ly) (* lx uy)) (max (* lx ly) (* ux uy)))]
                 [(and (< uy 0) (< ly 0))
                  (make-interval (* lx ly) (* ux uy))])]
          [(and (< ux 0) (< lx 0))
           (cond [(and (> uy 0) (> ly 0))
                  (make-interval (* lx uy) (* ux ly))]
                 [(and (> uy 0) (< ly 0))
                  (make-interval (* lx ly) (* ux uy))]
                 [(and (< uy 0) (< ly 0))
                  (make-interval (* ux uy))])])))

#?=(new-mul-interval r1 r2)
;ux > 0, lx > 0 かつ uy > 0, ly < 0のときに必要?