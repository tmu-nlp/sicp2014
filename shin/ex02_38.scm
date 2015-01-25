#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;nil → ()
#?=(fold-right / 1 (list 1 2 3))
#?=(fold-left / 1 (list 1 2 3))
#?=(fold-right list () (list 1 2 3))
#?=(fold-left list () (list 1 2 3))

;演算を行う順序に結果が左右されないopなので、どの並びに対しても同じ値
#?=(fold-right + 0 (list 1 2 3))
#?=(fold-left + 0 (list 1 2 3))
#?=(fold-right * 1 (list 1 2 3))
#?=(fold-left * 1 (list 1 2 3))