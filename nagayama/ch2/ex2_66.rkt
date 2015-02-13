#lang racket

; 二分木構造 のデータベースを実装する
; key値 が一致する要素を探す lookup 手続きを実装する
; 
; 木の要素を (key data) のセットに変更する
; 

; 新しく導入するセレクタなど
(define (key set-of-records) (caar set-of-records))
(define (data set-of-records) (cdar set-of-records))
(define (make-set key data) (cons key data))

; add-key-list : リストに keyを加える手続き
(define (add-key-list key lst)
  (if (null? lst)
      '()
      (cons (make-set key (car lst))
            (add-key-list (+ key 1) (cdr lst)))))

; make-data-set-tree : リストを key付き二分木構造にする手続き
(define (make-data-set-tree list)
  (list->tree (add-key-list 0 list)))

; lookup
(define (lookup given-key set)
  (cond ((null? set) #f)
        ((= given-key (key set))
         (data set))
        ((< given-key (key set))
         (lookup given-key (left-branch set)))
        (else
         (lookup given-key (right-branch set)))))


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
(define test-list '(And Bud Cond Do))
(define db-tree (make-data-set-tree test-list))
(define db-list (tree->list-2 db-tree))

(display "test-list : ")
test-list
(display "db-tree   : ")
db-tree
(display "db-list   : ")
db-list

(newline)

(define (lookup-test x)
  (display "[lookup-test]") (newline)
  (display "key  = ")
  (display x) (newline)
  (display "data = ")
  (lookup x db-tree))

(lookup-test 2)
