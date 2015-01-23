#lang racket

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; --- 答え ---
(define (make-segment start end)
  (list start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))

; --- テスト ---

(define v (make-vect 2 3))
(define s (make-segment v v))

(start-segment s)
'(2 3)

(end-segment s)
'(2 3)