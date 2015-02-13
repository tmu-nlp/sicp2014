#lang racket

; 2通りの tree->list の違い
; [a] 2つの tree->list は同じ結果を返すか
; [b] 2つの tree->list のステップ数増加のオーダーを比較

; append について参考にした URL
; http://saito.hatenablog.jp/entry/2014/03/28/184636
; 

; tree->list-1
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; tree->list-2
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; セレクタなど
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; run
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

; [a] 2つの tree->list は同じ結果を返すか
; 
; tree->list-1, tree->list-2 は同じ結果を返す
; 

; [b] 2つの tree->list のステップ数増加のオーダーを比較
; 
; それぞれの手続きを比較すると, 繰り返しの中で
; tree->list-1 では append を使っており,
; tree->list-2 では cons を使っている.
; 
; ステップ数増加のオーダーを考えると, 
; tree->list-1  --- O( N * log(N) )
; tree->list-2  --- O( N ) 
; である. これは, tree->list-1 は
; append によるリストのコピーが挟まるためである.
; 
; また, メモリ使用量のオーダーを考えると
; tree->list-1  --- O( N^2 )
; tree->list-2  --- O( N )
; であり, tree->list-2 の方が優秀な手続きである.
; 
; 
