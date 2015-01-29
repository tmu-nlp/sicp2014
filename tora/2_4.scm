#lang racket
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 1 2))

;(car (lambda (m) (m 1 2)))          ; consを展開
;
;((lambda (z) (z (lambda (p q) p)))  ; carを展開 ,zを置き換え
;    (lambda (m) (m 1 2)))
;
;((lambda (m) (m 1 2))               ; mを置き換え
;    (lambda (p q) p))
;
;((lambda (p q) p)                   ; pを置き換え
;    1 2)
;
;1

(define (cdr z)
    (z (lambda (p q) q)))
(cdr (cons 1 2))

;(cdr (lambda (m) (m 1 2)))          ; consを展開
;
;((lambda (z) (z (lambda (p q) q)))  ; cdrを展開 ,zを置き換え
;    (lambda (m) (m 1 2)))
;
;((lambda (m) (m 1 2))               ; mを置き換え
;    (lambda (p q) q))
;
;((lambda (p q) q) 1 2)              ; qを置き換え
;
;2
