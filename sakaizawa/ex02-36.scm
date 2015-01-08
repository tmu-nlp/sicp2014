#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-36.scm

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define items (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(print "(define items (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))")

(print "(accumulate-n + 0 items)")
(print (accumulate-n + 0 items))


