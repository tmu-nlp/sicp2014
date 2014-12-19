;ex1_32.scm
(define (inc x) (+ x 1))
(define (square x) (* x x))

;(define (product term a next b)
;	(if (> a b)
;		1
;		(* (term a) (product term (next a) next b))))

;(define (iter-product term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (* (term a) result))))
;  (iter a 1))

;accumulate recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
      
(define (sum term a next b)
  (accumulate + 0 term a next b))

(sum square 1 inc 3)

(define (product term a next b)
  (define null-value 1)
  (accumulate * 1 term a next b))

(product square 1 inc 3)


;accumulate iterative
(define (iter-accumulate combiner term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a 0))

(define (iter-sum term a next b)
  (define (combiner n m) (+ n m))
  (iter-accumulate combiner term a next b))

(iter-sum square 1 inc 3)

(define (iter-product term a next b)
  (define (combiner n m) (* n m))
  (iter-accumulate combiner term a next b))

(product square 1 inc 3)
  