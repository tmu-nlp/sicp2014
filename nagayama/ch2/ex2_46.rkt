#lang racket

; ペアで表現されたベクトルに対する演算手続き
; make-vect, xcor-vect, ycor-vect
; add-vect, sub-vect, scale-vect の定義


(define (make-vect x y) (cons x y))

(define (xcor-vect vec) (car vec))
(define (ycor-vect vec) (cdr vec))

(define (add-vect s t)
  (make-vect (+ (xcor-vect s) (xcor-vect t))
             (+ (ycor-vect s) (ycor-vect t))))
(define (sub-vect s t)
  (make-vect (- (xcor-vect s) (xcor-vect t))
             (- (ycor-vect s) (ycor-vect t))))
(define (scale-vect s vec)
  (make-vect (* s (xcor-vect vec))
             (* s (ycor-vect vec))))

; 以下、実行テスト

; ベクトル用意
(define a (make-vect 1 2))
(define b (make-vect 3 4))

; 出力
(display "a = ")
a
(display "b = ")
b
(display "a+b = ")
(add-vect a b)
(display "a-b = ")
(sub-vect a b)
(display "-a = ")
(scale-vect -1 a)