#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-12.scm

(define (pascal row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(print "(pascal 3 2)")
(print (pascal 3 2))

(print "(pascal 5 3)")
(print (pascal 5 3))


