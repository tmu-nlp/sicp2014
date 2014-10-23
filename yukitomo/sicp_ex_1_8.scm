;sicp_1_8.scm

(define (cbrt-iter guess pre-guess x)
	(if (good-enough? guess pre-guess)
		guess
		(cbrt-iter (improve guess x) guess x)))


(define (improve guess x)
    (/ (+ (/ x (* guess guess)) 
                (* 2 guess))
            3))


(define (good-enough? guess pre-guess)
    (< (/ (abs (- guess pre-guess)) guess) 0.0001))

(define (cbrt x)
	(cbrt-iter 1.0 100.0 x)) 

(print (cbrt 8))