#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-30.scm

(define (square-tree1 tree)
  (cond ((null? tree) ())
        ((not (pair? tree))
         (* tree tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (item)
         (if (pair? item)
             (square-tree2 item)
             (* item item)))
       tree))


(print "(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))")
(print (square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))))
(print "(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))")
(print (square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))))
