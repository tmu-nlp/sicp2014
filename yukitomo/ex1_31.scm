;ex1_31.scm
(define (inc x) (+ x 1))
(define (square x) (* x x))

;product
;recursive
(define (product term a next b)
	(if (> a b)
		1
		(* (term a) (product term (next a) next b))))

;iterative
(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;factorial:n!
(define (factorial n)
	(define (term a) a)
	(product term 1 inc n))

(factorial 3)
(factorial 4)

;pi (recursive)
(define (pi n)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1)) ;even
        (/ (+ n 1) (+ n 2)))) ;odd 
  (* 4.0 (product term 1 inc n)))
(pi 1000)

;pi (iterative)
(define (iter-pi n)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1)) ;even
        (/ (+ n 1) (+ n 2)))) ;odd 
  (* 4.0 (iter-product term 1 inc n)))
(iter-pi 1000)