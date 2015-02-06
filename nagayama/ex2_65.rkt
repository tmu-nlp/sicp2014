#lang racket

; バランスの取れた 二分木 の形をした集合について, 
; union-set : 和集合
; intersection-set : 積集合
; を O(N) で実装する.

#|
list->tree は O(N) であり, tree->list-2 も O(N) である. 
リスト同士の和集合, 積集合 手続きを O(N) で設計すれば良い.

union-list, intersection-list の
ステップ数増加のオーダーを確認すると,

(union-list N M) -> O( N+M )
(intersection-list N M) -> O( N+M )

であり, O(N)で実装されたと確認できた. 

|#

; 和集合 : union-set
(define (union-set tree1 tree2)
  (define (union-list lst1 lst2)
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          (else
           (let ((a (car lst1))
                 (b (car lst2)))
             (cond ((= a b)
                    (cons a (union-list (cdr lst1) (cdr lst2))))
                   ((< a b)
                    (cons a (union-list (cdr lst1) lst2)))
                   (else ; (> a b)
                    (cons b (union-list lst1 (cdr lst2)))))))))
  (list->tree (union-list (tree->list-2 tree1)
                          (tree->list-2 tree2))))

; 積集合 : intersection-set
(define (intersection-set tree1 tree2)
  (define (intersection-list lst1 lst2)
    (if (or (null? lst1) (null? lst2))
        '()
        (let ((a (car lst1))
              (b (car lst2)))
          (cond ((= a b)
                 (cons a (intersection-list (cdr lst1) (cdr lst2))))
                ((< a b)
                 (intersection-list (cdr lst1) lst2))
                (else ; (> a b)
                 (intersection-list lst1 (cdr lst2)))))))
  (list->tree (intersection-list (tree->list-2 tree1)
                                 (tree->list-2 tree2))))



; 以下、これまでに作成した手続き

; セレクタなどなど
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; tree->list-2 : 木をリストに変換
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

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
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))


; run
(define tree1 (list->tree '(1 2 3 4 5))) ; 5までの正整数
(define tree2 (list->tree '(2 3 5 7)))   ; 7までの素数

(display "tree1") (newline)
tree1
(display "tree2") (newline)
tree2
(newline)
(display "union-set") (newline)
(union-set tree1 tree2)
(display "intersection-set") (newline)
(intersection-set tree1 tree2)

