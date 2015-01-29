#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-28.scm

(define (fringe l)
  (if (pair? l)
      (if (pair? (car l))
          (append (fringe (car l)) (fringe (cdr l)))
          (cons (car l) (fringe (cdr l))))
      l))

(define x (list (list 1 2) (list 3 4)))
(print "(define x (list (list 1 2) (list 3 4)))")

(print "(fringe x)")
(print (fringe x))
(print "(fringe (list x x))")
(print (fringe (list x x)))



