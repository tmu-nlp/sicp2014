#lang racket
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (car x))
(define (lower-bound x)
  (cdr x))

(define (center-percent c p)
  (make-interval 
   (+ c (* c p))
   (- c (* c p))))

(define (make-center-percent i p)
  (center-percent 
   (center i)
   p))

(define x (make-interval 1 5)) 
(make-center-percent x 0.05)