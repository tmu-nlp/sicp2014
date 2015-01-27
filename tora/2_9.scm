#lang racket

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (car x))
(define (lower-bound x)
  (cdr x))

(define (make-width x) 
  (/ (+ (upper-bound x) (lower-bound x)) 2))


(define x (make-interval 0 6))
(define y (make-interval 2 4))
(- (make-width x) (make-width y))
(sub-interval x y)
(make-width (sub-interval x y))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

