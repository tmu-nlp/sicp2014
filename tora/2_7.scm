#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (car x))
(define (lower-bound x)
  (cdr x))


(define x (make-interval 1 2))
(define y (make-interval 3 4))
(add-interval x y)