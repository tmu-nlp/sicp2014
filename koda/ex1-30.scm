(use srfi-27)

(define (cube x)
  (* x x x))

;(define (sum term a next b)
;  (if (> a b)
;	0
;	(+ (term a)
;	   (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
	  result 
	  (iter (next a) (+ (term a) result))))
  (iter a 0))
	  
(print (sum-integers 1 10))
