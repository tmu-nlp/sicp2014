#lang planet neil/sicp

;; 与えられたリストの最後の要素だけからなるリストを返す手続きlast-pair

(define (last-pair items)
  (let ((next (cdr items)))
    (if (null? next)
        items
        (last-pair next))))

(last-pair (list 23 72 149 34)) 

;; -> 34