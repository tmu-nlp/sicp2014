;もとのgood-enoughではguessの2乗とxの差をとって比較しているため
;xが大きい場合は差が0.001になるまでとても時間がかかり，
;xが小さい場合はすぐに差が小さくなるため誤差が大きい．
;そこでguessの差分をgood-enoughの条件とすることで
;xの大小にかかわらずある程度の誤差&時間で解を求めることができる．
(define (sqrt-iter guess x)
    (if (good-enough? (improve guess x) guess)
	guess
	(sqrt-iter (improve guess x)
	    x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess guessBf)
    (< (abs (- guess guessBf)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(print
    (sqrt 10000000000000000000000000000000000000000000))
(print
    (sqrt 0.000000000001))
