#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;;実行の仕方
;;gosh Exercise1-5.scm

(print "Exercise1-5:")

(define (p) (p))

(define (test x y)
  (if (= x 0)
        0 y)) 

(print "applocative-order evalutionであればすぐにif文のオペランドを実行し0をprintするはずであるが実際はそうではなくオペランドの計算を行わず式を完全に展開しようとし(p)のループに入る。このことから評価はnomal-order evalutionで行われていると判断できる。")
(pirnt (test 0 (p)))
