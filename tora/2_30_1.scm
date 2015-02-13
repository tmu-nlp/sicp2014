#lang racket
(define (square x) (* x x))
(define (square-tree tree)
    (cond ((null? tree)                         ; 空树
            '())
          ((not (pair? tree))                   ; 叶子节点
            (square tree))
          (else 
            (cons (square-tree (car tree))
                  (square-tree (cdr tree))))))
(square-tree (list (list 1 2) (list 3 4)))