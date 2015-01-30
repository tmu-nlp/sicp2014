#lang scheme

; fold-right, fold-left
; 式の値を確認する
; 任意の列に対して同じ値を生成する条件

; fold-right
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define fold-right accumulate)

; fold-left
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


; run
(fold-right / 1.0 (list 1 2 3))
(fold-left / 1.0 (list 1 2 3))
(fold-right list (list) (list 1 2 3)) 
; (list 1 (list 2 (list 3 ())))
(fold-left list (list) (list 1 2 3))
; (list (list (list () 1) 2) 3)

; fold-right と fold-left とが同値になるのは
; op について 交換法則が成り立つとき

