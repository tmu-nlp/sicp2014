; #lang scheme

; a, b, c の 3 つの数のうち大きい 2 つの 2 乗和を出力.


(define (sum-of-squares x y)
        (+ (* x x) (* y y)))
(define (larger-sum-of-squares a b c)
        (cond (and (< a b) (< a c)) (sum-of-squares b c)
              (and (< b c) (< b a)) (sum-of-squares c a)
              (and (< c a) (< c b)) (sum-of-squares a b))) 
