#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (fringe l)
  (if (pair? l)
      (if (pair? (car l))
          (append (fringe (car l)) (fringe (cdr l)))
          (cons (car l) (fringe (cdr l))))
      l))

(define x (list (list 1 2) (list 3 4)))
#?=x
#?=(fringe x)
#?=(fringe (list x x))



