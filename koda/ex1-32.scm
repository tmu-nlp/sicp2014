(use srfi-27)

(define (double x)
  (* x x))
(define (cube x)
  (* x x x))

(define (inc n) (+ n 1))
(define (identity x) x)

;再帰
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b) 
	  result 
	  (iter (next a) (combiner (term a) result))))
  (iter a null-value))
;線形
(define (accumulate-int combiner null-value term a next b)
  (if (> a b)
	null-value	
	(combiner (term a)
	   (accumulate-int combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (sum-int term a next b)
  (accumulate-int + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))
(define (product-int term a next b)
  (accumulate-int * 1 term a next b))

(print (sum identity 1 inc 100))
(print (sum-int identity 1 inc 100))
(print (product identity 1 inc 10))
(print (product-int identity 1 inc 10))
