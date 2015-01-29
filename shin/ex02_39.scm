#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
(load "./ex02_38.scm")
;----------------------------------------
;(define (fold-right op initial sequence)
;  (if (null? sequence)
;      initial
;      (op (car sequence)
;          (fold-right op initial (cdr sequence)))))
;
;(define (fold-left op initial sequence)
;  (define (iter result rest)
;    (if (null? rest)
;        result
;        (iter (op result (car rest))
;              (cdr rest))))
;  (iter initial sequence))
;------------------------------------------

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))

(print "-----ex02_39-----")
;fold-right
#?=(reverse-1 (list 1 2 3))
;fold-left
#?=(reverse-2 (list 1 2 3))


