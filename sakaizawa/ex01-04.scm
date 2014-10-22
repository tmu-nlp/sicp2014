#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-04.scm

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) 

(print "bの値が正だと足し算で負だと引き算を行う")
(print )

(print "(a-plus-abs-b 3 4)")
(print (a-plus-abs-b 3 4))
(print )

(print "(a-plus-abs-b 3 -2)")
(print (a-plus-abs-b 3 -2))

