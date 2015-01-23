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
;(define r1 (make-interval 6 8))
;(print "(define r1 (make-interval 6 8))")
;(define r2 (make-interval 7 9))
;(print "(define r2 (make-interval 7 9))")
;(define r3 (make-interval 5 10))
;(print "(define r3 (make-interval 5 10))")
;(define r4 (make-interval -5 10))
;(print "(define r4 (make-interval -5 10))")
;
;#?=(add-interval r1 r2)
;#?=(mul-interval r1 r2)
;#?=(div-interval r1 r2)
;---------------------------------------------------

;(div-interval r3 r4)がほんとは(-1, 1)になってほしいが乗算を行ったあとに全体のmax,minをとっているため，(-2,1)となる.
;そこでエラーを出す形に直す

(define (div-interval x y)
  (let ((a (lower-bound y))
        (b (upper-bound y)))
  (if (< (* a b) 0)
      (error "error")
      (mul-interval x
                    (make-interval (/ 1.0 a)
                                   (/ 1.0 b))))))
#?=(div-interval r3 r4)
