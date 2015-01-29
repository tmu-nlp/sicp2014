#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-18.scm

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (null? l)
      ()
      (append (reverse (cdr l)) (list (car l)))))


(print "(reverse (list 1 4 9 16 25))")
(print (reverse (list 1 4 9 16 25)))


