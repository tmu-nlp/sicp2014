#lang scheme

; ex2.30 の結果を一般化して tree-map を作る
; tree-map を用いて square-tree を計算

(define (tree-map proc tree)
  (if (null? tree)
      tree
      (if (pair? tree)
          (cons (tree-map proc (car tree))
                (tree-map proc (cdr tree)))
          (proc tree))))


(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

; run
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; (1 (4 (9 16) 25) (36 49))
