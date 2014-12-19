(use srfi-27)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (inc n) (+ n 1))
(define (product-cubes a b)
  (product cube a inc b))
(define (identity x) x)
(define (product-integers a b)
  (product identity a inc b))
(define (sum-integers a b)
  (sum identity a inc b))

;再帰
(define (filtered-accumulate filter  combiner null-value term a next b)
  (define (iter a result)
	(if (> a b) 
	  result 
	  (if (filter a)
		(iter (next a) (combiner (term a) result))
		(iter (next a) (combiner null-value result)))))
  (iter a null-value))
;線形
(define (filtered-accumulate-int filter combiner null-value term a next b)
  (if (> a b)
	null-value	
	(if (filter a)
	  (combiner (term a)
	   (filtered-accumulate-int filter combiner null-value term (next a) next b)))))

(define (sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))
(define (sum-int filter term a next b)
  (filtered-accumulate-int filter + 0 term a next b))
(define (product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))
(define (product-int filter term a next b)
  (filtered-accumulate-int filter * 1 term a next b))

(define (product-su n)
  (define (su x)
	(= 1 (gcd x n)))
  (product su identity  1 inc n))

(print (sum prime? square 2 inc 10))
(print (product-su 10))
