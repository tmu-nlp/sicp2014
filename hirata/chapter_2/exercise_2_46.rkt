#lang racket

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect vect-1 vect-2)
  (make-vect
   (+ (xcor-vect vect-1) (xcor-vect vect-2))
   (+ (ycor-vect vect-1) (ycor-vect vect-2))))

(define (sub-vect vect-1 vect-2)
  (make-vect
   (- (xcor-vect vect-1) (xcor-vect vect-2))
   (- (ycor-vect vect-1) (ycor-vect vect-2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 5))
(display (add-vect v1 v2))
(newline)
(display (add-vect v1 v2))
(newline)
(display (sub-vect v1 v2))
(newline)
(display (scale-vect 5 v1))
