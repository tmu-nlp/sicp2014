#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-60.scm

;element-of-set?、intersection-set は変更なし

(define (adjoin-set x set)
  (cons x set))

#?=(adjoin-set '1 '(1 2 3 4 5))

(define (union-set set1 set2)
  (append set1 set2))

#?=(union-set '(1 2 3 4 5) '(2 4 6))

;重複を許すと出力のリストが大きくなるので、効率は悪くなる

