#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-38.scm

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
(print "(fold-right / 1 (list 1 2 3))")
(print (fold-right / 1 (list 1 2 3)))
(print "(fold-left / 1 (list 1 2 3))")
(print (fold-left / 1 (list 1 2 3)))
(print "(fold-right list () (list 1 2 3))")
(print (fold-right list () (list 1 2 3)))
(print "(fold-left list () (list 1 2 3))")
(print (fold-left list () (list 1 2 3)))
(print )

(print "演算を行う順序に結果が左右されないような op の場合、どのような並びに対しても同じ値を生じる")
(print "(fold-right + 0 (list 1 2 3))")
(print (fold-right + 0 (list 1 2 3)))
(print "(fold-left + 0 (list 1 2 3))")
(print (fold-left + 0 (list 1 2 3)))
(print "(fold-right * 1 (list 1 2 3))")
(print (fold-right * 1 (list 1 2 3)))
(print "(fold-left * 1 (list 1 2 3))")
(print (fold-left * 1 (list 1 2 3)))

