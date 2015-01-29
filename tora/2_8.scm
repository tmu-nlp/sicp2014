#lang racket

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (car x))
(define (lower-bound x)
  (cdr x))



(define x (make-interval 0 6))
(define y (make-interval 2 4))
(sub-interval x y)