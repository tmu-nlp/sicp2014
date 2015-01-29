#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-17.scm

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(print "(last-pair (list 23 72 149 34))")
(print (last-pair (list 23 72 149 34)))


