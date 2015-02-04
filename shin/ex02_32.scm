#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (subsets a)
  (if (null? a)
      (list ())
      (let ((rest (subsets (cdr a))))
        (append
         rest
         (map (lambda (x) (cons (car a) x))
                      rest)))))

(define a (list 1 2 3))
(define b (list 1 2 3 4))
#?=a
;#?=(car a)
;#?=(cdr a)
#?=(subsets a)
#?=b
#?=(subsets b)


