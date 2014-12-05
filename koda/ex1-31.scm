(use srfi-27)

(define (double x)
  (* x x))
(define (cube x)
  (* x x x))

(define (inc n) (+ n 1))
(define (product-cubes a b)
  (product cube a inc b))
(define (identity x) x)
(define (product-integers a b)
  (product identity a inc b))

;再帰
(define (product term a next b)
  (define (iter a result)
	(if (> a b) 
	  result 
	  (iter (next a) (* (term a) result))))
  (iter a 1))
;線形
(define (product-integral term a next b)
  (if (> a b)
	1
	(* (term a)
	   (product-integral term (next a) next b))))


(define (func x)
  (/ (double (+ x 1)) (double x)))
(define (inc2 n) (+ n 2))
(define (product-pi n)
  (* 8.0 (/ (product-integral func 3.0 inc2 n) (+ n 2.0))))

(print (product-pi 101))
(print (product-pi 1001))
(print (product-pi 10001))
(print (product-pi 100001))
