再帰的な手続きは、
(define (recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
	((= n 2) 2)
        (else (+ (recursive (- n 1))
              	 (* 2 (recursive (- n 2)))
		 (* 3 (recursive (- n 3)))
	      )
	)
  )
)
であり、反復的な手続きは、
(define (iteration n)
  (fib-iter2 2 1 0 n))

(define (fib-iter2 a b c count)
  (if (= count 0)
      c
      (fib-iter2 (+ a (* 2 b) (* 3 c)) a b (- count 1))))
となる。