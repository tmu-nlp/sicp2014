#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-14.scm

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


(define r (make-center-percent 10 1))
(print "(define r (make-center-percent 10 1))")
(print )
(print "(percent r)")
(print (percent r))
(print )
(print "(percent (div-interval r r))")
(print (percent (div-interval r r)))
(print )



