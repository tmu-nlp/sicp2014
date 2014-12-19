#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-15.scm

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)) 
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)) 

(define (make-center-percent c p)
  (make-interval (- c (* (/ c 100.0) p)) 
                 (+ c (* (/ c 100.0) p))))
(define (percent i)
  (* (/ (width i) (center i)) 100.0))

(define (make-interval a b) (cons a b)) 
(define (lower-bound x) (car x)) 
(define (upper-bound x) (cdr x))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r (make-center-percent 10 1))
(print "(define r (make-center-percent 10 1))")
(print "(percent (par1 r r))")
(print (percent (par1 r r)))
(print "(percent (par2 r r))")
(print (percent (par2 r r)))

