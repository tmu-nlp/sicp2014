#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-35.scm

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (cond ((null? x) 0)
                                         ((not (pair? x)) 1)
                                         (else (count-leaves x)))) t)))

(define x (cons (list 1 2) (list 3 4)))

(print "(count-leaves x)")
(print (count-leaves x))

