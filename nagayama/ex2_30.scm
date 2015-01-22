#lang scheme

; square-tree : map を利用して木の各要素を2乗する

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
         tree))

(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))


(define (square x) (* x x))

; run
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; (1 (4 (9 16) 25) (36 49))