#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (square x) (* x x))

;再帰の場合
(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))

;mapの場合
(define (square-list-map items)
  (map square items))

#?=(square-list (list 1 2 3 4))
#?=(square-list-map (list 1 2 3 4))

