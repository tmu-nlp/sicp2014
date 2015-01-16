#!/usr/bin/env gosh
; -*- coding: utf-8; -*-

(define (div-interval x y)
  (if (= 0 (* (upper-bound y) (lower-bound y)))
      (display "error")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(print "(div-interval (cons 10 100)(cons 10 200))")
(print (div-interval (cons 10 100)(cons 10 200)))

(print "(div-interval (cons 10 100)(cons 0 200))")
(print (div-interval (cons 10 100)(cons 0 200)))