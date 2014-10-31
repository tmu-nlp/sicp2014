#!/usr/local/bin/gosh

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(print (test 0 (p)))	;適用順序評価では最初の(define (p) (p))が無限ループで処理できない。正規順序評価ではifの部分でpには触れないのでちゃんと評価できる。
;実行すると終わらない。
