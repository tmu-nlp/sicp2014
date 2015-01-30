#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-61.scm

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else
          (cons (car set) (adjoin-set x (cdr set))))))

#?=(adjoin-set '1 '(2 3 4))
#?=(adjoin-set '2 '(2 3 4))
#?=(adjoin-set '5 '(2 3 4))

;最大の場合は集合の大きさのn ステップ数になるが、平均するとn/2 のステップ数

