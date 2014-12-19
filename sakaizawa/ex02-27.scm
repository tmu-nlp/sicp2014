#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-27.scm

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (null? l)
      ()
      (append (reverse (cdr l)) (list (car l)))))

(define (deep-reverse items)
  (if (pair? items)
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))
      items))

(define x (list (list 1 2) (list 3 4)))
(print "(define x (list (list 1 2) (list 3 4)))")
(print x)
(print "(reverse x)")
(print (reverse x))
(print "(deep-reverse x)")
(print (deep-reverse x))
