#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

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
#?=x
#?=(reverse x)
#?=(deep-reverse x)
#?=(deep-reverse (list (list 66 1 2) (list 55 3 4)))
