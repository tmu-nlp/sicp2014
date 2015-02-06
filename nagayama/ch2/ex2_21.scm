#lang scheme

; square-list 手続きを map を使わない場合と使った場合の2つ作る

(define (map proc items)
  (if (null? items)
      (list)     ; nil が無いので変更
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))

#|
(define (square-list items)
  (if (null? items)
      (list)    ; nil が無いので変更
      (cons (square (car items))
            (square-list (cdr items)))))
|#

(define (square-list items)
  (map (lambda (x) (square x)) items))

(square-list (list 1 2 3 4))