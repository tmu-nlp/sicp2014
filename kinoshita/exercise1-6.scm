;new-ifのpredicateがtrueでも，elseの方を評価しようとするために
;sqrt-iterを無限に呼び出すことになりうまく動作しないからifをspecial form
;としている
(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
	  (else else-clause)))

(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x)
	    x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(print
    (sqrt 9))
