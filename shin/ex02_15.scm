#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
(load "./ex02_07.scm")
;---------------------------------
;(define (add-interval x y)
;  (make-interval (+ (lower-bound x) (lower-bound y))
;                 (+ (upper-bound x) (upper-bound y))))
;(define (mul-interval x y)
;  (let ((p1 (* (lower-bound x) (lower-bound y)))
;        (p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y)))
;        (p4 (* (upper-bound x) (upper-bound y))))
;    (make-interval (min p1 p2 p3 p4)
;                   (max p1 p2 p3 p4))))
;(define (div-interval x y)
;  (mul-interval x 
;                (make-interval (/ 1.0 (upper-bound y))
;                               (/ 1.0 (lower-bound y)))))
;-----------------------------------

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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

#?=(make-center-percent 10 1)
(define r (make-center-percent 10 1))
#?=(percent (par1 r r))
#?=(percent (par2 r r))
;; 他の場合と比較して単純な誤差の和になっていない.
;; 誤差の大きさが大きく異なる場合、
;; 小さい方の誤差の影響はほとんど無視出来るほど小さくなることが予想される.

;; 以上から、par1よりpar2のほうが「より良い」プログラムであるというのは真.
;; par2は双方ともに区間を用いるのではなく、 
;; 片方の区間を定数(誤差0)にすることで相対誤差の増加を抑えているため.

