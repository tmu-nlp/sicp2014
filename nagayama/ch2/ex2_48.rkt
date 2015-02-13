#lang racket

; 位置ベクトルを用いて線分を表現する手続き
; make-segment, start-segment, end-segment の定義

; 線分
(define (make-segment s t) (cons s t))
(define (start-segment edge) (car edge))
(define (end-segment edge) (cdr edge))

; ベクトル
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

(define ab (make-segment a b))

ab
(start-segment ab)
(end-segment ab)
