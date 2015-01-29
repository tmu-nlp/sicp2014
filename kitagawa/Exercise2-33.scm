#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-33.scm

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(print "(map square (list 1 2 3 4 5))")
(print (map square (list 1 2 3 4 5)))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(print "(append (list 1 2) (list 3 4))")
(print (append (list 1 2) (list 3 4)))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


(print "(length (list 1 2 3 4 5))")
(print (length (list 1 2 3 4 5)))

