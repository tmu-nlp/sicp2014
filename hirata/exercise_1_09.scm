#!/usr/local/bin/gosh

;Exercise 1.9: 次の 2 つの各手続は 2 つの正の整数を加算する手段を定義している。手続 inc は引数を 1 増やし、dec は引数を 1 減 らす。
;(define (+ a b)
;(if (= a 0) b (inc (+ (dec a) b))))
;(define (+ a b)
;(if (= a 0) b (+ (dec a) (inc b))))
;置換モデルを用いて各手続が (+ 4 5) の評価において生成するプロセスを図示せよ。これらのプロセスは反復であるか、再帰であるか?

(define (+ a b)
	(if (= a 0) b (inc (+ (dec a) b))))

;置き換えてみる
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9									;再起

(define (+ a b)
	(if (= a 0) b (+ (dec a) (inc b))))

;同様に置き換えてみる
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9									;反復
