#lang racket

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree-map tree)
  (map (lambda (sub-tree) 
         (if (pair? sub-tree) 
             (square-tree-map sub-tree)
             (sqr sub-tree)))
       tree))

(define (square-tree tree) (tree-map sqr tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
'(1 (4 (9 16) 25) (36 49))