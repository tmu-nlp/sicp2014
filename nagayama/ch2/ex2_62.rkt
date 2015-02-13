#lang racket

; 順序付けられたリスト形式の集合による和集合
; union-set

(define (union-set s1 s2)
   (cond 
     ((and (null? s1) (null? s2)) '())
     ((null? s1) s2)
     ((null? s2) s1)
     ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
     ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))
     (else (cons (car s1) (union-set (cdr s1) (cdr s2))))))


; run

(union-set '() '())
(union-set '(1 2 3) '())
(union-set '(1 2 3) '(1 2 3))
(union-set '(1 2 3) '(0 2 9))

