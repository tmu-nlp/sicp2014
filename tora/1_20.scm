#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;test
(gcd 206 40)

;正規順序なら
;  (gcd 206 40)
;->(gcd (remainder 206 40))
;->(gcd (remainder 206 40) (remainder 40 remainder(206 40)) )
;->...無限ループになる？×
;if文の条件判断は解釈順序を問わず必ず評価するから
;(if (= 40 0))
;   206
;   (gcd (...))
;
;作用的順序なら
;  (gcd 206 40)
;->(gcd 40 (remainder 206 40))
;->(gcd 40 6)
;->(gcd 6 (remainder 40 6))
;->(gcd 6 4)
;->(gcd 4 (remainder 6 4))
;->(gcd 4 2)
;->(gcd 2 (remainder 4 2))
;->(gcd 2 0)
;-> 2
