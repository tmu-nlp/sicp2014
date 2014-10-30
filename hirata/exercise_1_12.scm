#!/usr/local/bin/gosh

(define (pascal n k)
    (if (or (= n k) (= k 1))
		1
		(+ (pascal (- n 1) (- k 1))
		     (pascal (- n 1) k))))

(print (pascal 7 2))
