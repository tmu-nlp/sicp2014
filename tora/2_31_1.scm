#lang racket

(define (tree-map f tree)
    (cond ((null? tree)                         ; 空树
            '())
          ((not (pair? tree))                   ; 叶子节点
            (f tree))
          (else
            (cons (tree-map f (car tree))       ; 递归处理左右子树
                  (tree-map f (cdr tree))))))

(define (square x) (* x x))
(tree-map square (list (list 1 2) (list 3 4)))

