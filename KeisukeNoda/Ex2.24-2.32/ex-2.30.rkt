#lang racket

;; 再帰を利用したもの
(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (sqr tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
'(1 (4 (9 16) 25) (36 49))


;; mapを利用したもの
(define (square-tree-map tree)
  (map (lambda (sub-tree) 
         (if (pair? sub-tree) 
             (square-tree-map sub-tree)
             (sqr sub-tree)))
       tree))


(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
'(1 (4 (9 16) 25) (36 49))