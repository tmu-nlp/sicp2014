#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;実行の仕方
;gosh Exercise1-1.scm

;最初なのでHello Worldのテスト

(print "Hello World")
(print "SICPの勉強会です")


(print 10)
(print (+ 5 3 4))
(print (- 9 1))
(print (/ 6 2))
(print (+ (* 2 4) (- 4 6)))
(define a 3)
(define b (+ a 1))

(print (+ a b (* a b)))
(print (= a b))

;;if文の書き方 (if 条件式 真の式 偽の式)

(if (and (> b a) (< b (* a b)))
    (print b)
    (print a))

;;condはC言語でいうswitch的なもの

(cond ((= a 4) (print 6))
      ((= b 4) (print (+ 6 7 a)))
      (else (print 25)))

(print (+ 2 (if (> b a) b a)))

(print (* (cond ((> a b) a)
         ((< a b) b)
                  (else -1))
   (+ a 1)))
