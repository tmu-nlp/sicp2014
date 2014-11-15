(use srfi-27)
(define (expmod base exp m)
  (cond ((= exp 0)
		 1)
		((even? exp)
		 (remainder
		   (square
			 (expmod base (/ exp 2) m))
		   m))
		(else
		  (remainder
			(* base
			   (expmod base (- exp 1) m))
			m))))
;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

(print (expmod 2 10 10))

