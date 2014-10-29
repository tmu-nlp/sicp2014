;sicp_ex_1_6.scm

(define (new-if predicate then-clause else-clause) 
   (cond (predicate then-clause)
   (else else-clause)))

(define (sqrt-iter guess x)
   (new-if (good-enough? guess x)
    guess (sqrt-iter (improve guess x) x)))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(sqrt 36)
;(if <predicate> <consequent> <alternative>)
;「特殊形式ifの評価規則は、解釈系が正規順序と作用的順序のどちらを使うかに無関係に同じとする。述語式を最初に評価し、その結果が帰結式と代替式のいずれを評価するかを決める。」(p.12)
;if式を使わずにcondで実装したことにより、以上の性質が利用できない。問題文中で定義されているsqrt-iterでは、new-ifに作用させるために(sqrt-iter (improve guess x) x) が無限に呼ばれてしまう。
;if 式を評価するためにインタプリタは、式の<predicate>の部分を評価することからはじめ、もし<predicate>が真ならば、<consequent>の値を返し、<alternative>は評価しないため