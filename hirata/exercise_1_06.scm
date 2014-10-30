#!/usr/local/bin/gosh

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;    guess
;    (sqrt-iter (improve guess x) x)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
;(print (new-if (= 2 3) 0 5))

(print (sqrt 9))	;よくわからないけどまた無限ループらしい。new-ifで実行すると終わらない。new-ifで実装するとどちらの評価も実行してしまうので、else側のsqrt-iterを再起し続けてしまって終わらない。
