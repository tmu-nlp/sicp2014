#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set s1 s2)
  (cond 
    ((and (null? s1) (null? s2)) '())
    ((null? s1) s2)
    ((null? s2) s1)
    ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
    ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))
    (else (cons (car s1) (union-set (cdr s1) (cdr s2))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
    (copy-to-list tree '()))

(define (list->tree elements) 
  (car (partial-tree elements (length elements)))) 
 
(define (partial-tree elts n) 
  (if (= n 0) 
      (cons '() elts) 
      (let ((left-size (quotient (- n 1) 2))) 
        (let ((left-result (partial-tree elts left-size))) 
          (let ((left-tree (car left-result)) 
                (non-left-elts (cdr left-result)) 
                (right-size (- n (+ left-size 1)))) 
            (let ((this-entry (car non-left-elts)) 
                  (right-result (partial-tree (cdr non-left-elts) 
                                              right-size))) 
              (let ((right-tree (car right-result)) 
                    (remaining-elts (cdr right-result))) 
                (cons (make-tree this-entry left-tree right-tree) 
                      remaining-elts))))))))


;; 順序づけられたリストによる集合の union-set、intersection-set を利用する。
;; 木構造データをリストに変換してから演算を行い、結果のリストを木構造データに変換する。

;------------------ 答え --------------------

(define (union-tree tree1 tree2)
  (let ((s1 (tree->list tree1))
        (s2 (tree->list tree2)))
    (list->tree (union-set s1 s2))))

(define (intersection-tree tree1 tree2)
  (let ((s1 (tree->list tree1))
        (s2 (tree->list tree2)))
    (list->tree (intersection-set s1 s2))))

;------------------ テスト --------------------

(define set1 '(1 2 3 4 5))
(define set2 '(2 4 6))
(define tree1 (list->tree set1))
; (3 (1 () (2 () ())) (4 () (5 () ())))
(define tree2 (list->tree set2))
; (4 (2 () ()) (6 () ()))

  ;; 1 ;;
;;        3
;;       /|
;;      1 4
;;     /  |
;;    2   5

  ;; 2 ;;
;;        4
;;       /|
;;      2 6

(union-tree tree1 tree2)
;;        3
;;       /|
;;      1 5
;;     / /|
;;    2 4 6

(intersection-tree tree1 tree2)
;;        2
;;       /
;;      4