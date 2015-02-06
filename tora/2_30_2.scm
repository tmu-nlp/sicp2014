#lang racket
(define (square x) (* x x))
(define (square-tree tree)
    (map (lambda (sub-tree)
             (if (pair? sub-tree)           ; 如果有左右子树
                 (square-tree sub-tree)     ; 那么递归地处理它们
                 (square sub-tree)))
         tree))
(square-tree (list (list 1 2) (list 3 4)))