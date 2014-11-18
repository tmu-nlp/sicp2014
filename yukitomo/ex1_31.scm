;ex1_31.scm
(define (inc x) (+ x 1))
(define (square x) (* x x))

;product
(define (product term a next b)
	(if (> a b)
		1
		(* (term a) (product term (next a) next b))))

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

;pi/4
(define (pi/4 n)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (* 1 (product term 1 inc n)))

(pi/4 100)