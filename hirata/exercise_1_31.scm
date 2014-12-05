;exercise 1.31

(define (product term a next b)
	(if (> a b)
            1
	    (* (term a)
	      (product term (next a) next b))))

;(define (factorial n)
;	(define (inc n) (+ n 1))
;	(product identity 1 inc n))

(define (pi4 n)
	(define (inc n) (+ n 1))
	(define (square n) (* n n))
	(define (term a)
		(define 2a (* a 2))
		(/ (* 2a (+ 2a 2)) (square (+ 2a 1))))
	(product term 1 inc n))
 
(display (* 4.0 (pi4 100)))