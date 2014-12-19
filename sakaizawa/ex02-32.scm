#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-32.scm

(define (subsets a)
  (if (null? a)
      (list ())
      (let ((rest (subsets (cdr a))))
        (append
         rest
         (map (lambda (x) (cons (car a) x))
                      rest)))))

(define a (list 1 2 3))
(print "(define a (list 1 2 3))")
(print "(subsets a)")
(print (subsets a))
(define b (list 1 2 3 4))
(print "(define b (list 1 2 3 4))")
(print "(subsets b)")
(print (subsets b))


