#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-21.scm

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

(print "(square-list (list 1 2 3 4))")
(print (square-list (list 1 2 3 4)))
(print )
(print "(square-list-map (list 1 2 3 4))")
(print (square-list-map (list 1 2 3 4)))

