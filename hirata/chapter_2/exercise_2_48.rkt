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
;ここまでコピペ


;ここから2-48
(define make-segment cons)
(define (start-segment s)
  (car s))
(define (end-segment s)
  (add-vect (start-segment s) (cdr s)))

(define s (make-segment (make-vect 1 2) (make-vect 5 7)))
(display (start-segment s))
(newline)
(display (end-segment s))


