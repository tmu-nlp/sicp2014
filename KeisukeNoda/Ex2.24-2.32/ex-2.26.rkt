#lang racket

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;-> '(1 2 3 4 5 6)
(cons x y)   ; -> '((1 2 3) 4 5 6)
(list x y)   ; -> '((1 2 3) (4 5 6))

;; append はリストのトップレベルの要素をバラけさせて連結する
;; cons は第2引数に指定したリストの先頭に第1引数のリストを挿入する
;; list は引数を全て要素としてリストを生成する