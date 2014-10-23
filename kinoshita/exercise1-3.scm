(define (square x) (* x x))

(define a 3)
(define b 4)
(define c 5)
(print
    (cond ((and (> a c) (> b c)) (+ (square a) (square b)))
	((and (> a b) (> c b)) (+ (square a) (square c)))
	(else (+ (square b) (square c)))
    )
)
