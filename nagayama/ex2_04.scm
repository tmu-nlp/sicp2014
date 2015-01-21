#lang scheme

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

; (car (cons x y))
;  は x を返す

; 確認
;    (car (cons x y))
; -> ((cons x y) (lambda (p q) p))   (carの定義より
; -> ((lambda (p q) p) x y)          (consの定義より
; -> x                               (展開

(define (cdr z)
  (z (lambda (p q) q)))

; (cdr (cons 1 2))