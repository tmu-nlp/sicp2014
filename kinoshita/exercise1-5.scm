;normal-orderだと引数を先に評価しようとするため，(p)の評価で無限ループ
;となる．applivative-orderではオペレータから評価するためif文で0を返し
;終了する.
(define (p) (p))

(define (test x y)
    (if (= x 0)
	0
	y))

(print
    (test 0 (p)))
