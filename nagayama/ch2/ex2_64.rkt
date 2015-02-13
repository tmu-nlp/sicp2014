#lang racket

; list->tree の手続きの実装1
; [a] partial-tree 手続きの動きの解説, 
;     リスト(1 3 5 6 9 11) を代入したときの木を図示する
; [b] ステップ数の増加オーダーはいくらか

; list->tree : リストをバランスのとれた木に変換
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size      (quotient (- n 1) 2))
             (left-result    (partial-tree elts left-size))
             (left-tree      (car left-result))
             (non-left-elts  (cdr left-result))
             (right-size     (- n (+ left-size 1)))
             (this-entry     (entry non-left-elts))
             (right-result   (partial-tree (cdr non-left-elts) right-size))
             (right-tree     (car right-result))
             (remaining-elts (cdr right-result)))
        #| 途中経過出力
        (display "elts: ") (display elts) (newline)
        (display "n   : ") (display n) (newline)
        (display "left-size: ") (display left-size) (newline)
        (display "left-result: ") (display left-result) (newline)
        (display "right-result: ") (display right-result) (newline)
        (display "this-entry: ") (display this-entry) (newline)
        (display "left-tree: ") (display left-tree) (newline)
        (display "right-tree: ") (display right-tree) (newline)
        (display (cons (make-tree this-entry left-tree right-tree)
                       remaining-elts)) (newline)
        (newline)
        ; |#
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))


; セレクタなどなど
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))


;; [a] partial-tree 手続きの動きの解説, 
;;     リスト(1 3 5 6 9 11) を代入したときの木を図示する

#|
< partical-tree の動き > 
1. リストとその長さを引数に取る
2. リストの長さが0 ならば nilを返す 
3. リストの中央の要素をノードにする(偶数個なら左側優先)
4. ノードより左側, ノードより右側, それぞれ再帰する
5. 末端部から順に make-tree で木構造にまとめる

< 具体例 >

'(1 2 3 4)
-> node  : 2
   right : '(1)  --> node  : 1
   left  : '(3 4)    left  : '()
             \       right : '()
              \
               --> node  : 3
                   left  : '()
                   right : '(4) --> node  : 4
                                    left  : '()
                                    right : '()

; |#

; 実行テスト

(define (test tree)
  (display "input -> ")
  (display tree) (newline)
  (newline)
  (list->tree tree))

(test '(1 3 5 7 9 11))
; -> '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))


#|

  木構造の図示

        5
      /   \
    1       9
   / \     /  \
  () 3     7   11
    / \   / \  / \
   () () () ()() ()

|#


;; [b] ステップ数の増加オーダーはいくらか

#|

partial-tree が呼び出される回数は 木の要素数と等しい.
つまり, nil のときを含めても高々 (2n + 1)回 である. 
よって, ステップ数の増加オーダーは O(N)


; |#




; partial-tree-origin : テキスト記載の partial-tree
(define (partial-tree-origin elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts
                                         left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (entry non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))



