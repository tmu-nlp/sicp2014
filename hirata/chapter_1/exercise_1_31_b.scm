;exercise 1.32

;再帰プロセス
;(define (product term a next b)
;	(if (> a b)
;           1
;	    (* (term a)
;	      (product term (next a) next b))))

;線形プロセス
(define (product term a next b)
	(define (iter a result)
		(if (> a b)
		    result
    		    (iter (next a) (* (term a) result))))
	(iter a 1))

(define (pi4 n)
	(define (inc n) (+ n 1))
	(define (square n) (* n n))
	(define (term a)
		(define 2a (* a 2))
		(/ (* 2a (+ 2a 2)) (square (+ 2a 1))))
	(product term 1 inc n))
 
(display (* 4.0 (pi4 100)))