#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;;実行の仕方
;;gosh Exercise1-4.scm

(print "Exercise1-4:")
(print "----------\nObserve that our model of evaluation allows for combinations whose operators are\ncompound expressions. Use this observation to describe the behavior of the following procedure:\n(define (a-plus-abs-b a b)\n     ((if (> b 0) + -) a b))\n----------")


(define (a-plus-abs-b a b)
 ((if (> b 0) + -) a b))
(print "Answer:")
(print "----------\nif文が真のとき、演算子 + 、偽のとき、 - が選ばれその後a bに対してそれが実行されるのでa+|b|を返す関数である。\n----------")
(print (a-plus-abs-b 1 1))
(print (a-plus-abs-b 1 -1))

