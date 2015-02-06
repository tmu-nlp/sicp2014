#lang scheme

; 2つのリストを接続する手続きの比較
; append, cons, list

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
  ; (1 2 3 4 5 6)
  ; 要素数 7 のリスト
  ; x の空要素が y に置き換わった形

(cons x y)
  ; ((1 2 3) 4 5 6)
  ; 要素数 5 のリスト
  ; (x . y)

(list x y)
  ; ((1 2 3) (4 5 6))
  ; 要素数 3 のリスト 
  ; (x . (y . ()))
