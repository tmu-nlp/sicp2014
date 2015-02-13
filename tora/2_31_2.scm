#lang racket
(define (square x) (* x x))
(define (tree-map f tree)
    (map (lambda (sub-tree)
             (if (pair? sub-tree)
                 (tree-map f sub-tree)  ; 处理子树
                 (f sub-tree)))         ; 处理节点
         tree))

(define (square x) (* x x))
(tree-map square (list (list 1 2) (list 3 4)))