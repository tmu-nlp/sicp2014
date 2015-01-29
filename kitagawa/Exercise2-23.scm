#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-23.scm

(define (for-each func items)
  (cond ((null? items) #t)
        (else (func (car items))
              (for-each func (cdr items)))))

(print "(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))")
(print (for-each (lambda (x) (newline) (display x))
          (list 57 321 88)))

