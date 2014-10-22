(define (sqrt-iter guess x)
    (if (good-enough? (improve guess x) guess)
	guess
	(sqrt-iter (improve guess x)
	    x)))

(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess guessBf)
    (< (abs (- guess guessBf)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(print
    (sqrt 27))
(print
    (sqrt 8))
