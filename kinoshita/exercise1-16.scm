(define (fast-expt b n a)
    (cond
	((= n 0) a)
	((even? n) (fast-expt (square b) (/ n 2) a))
	(else (fast-expt b (- n 1) (* a b)))))

(define (even? n)
    (= (remainder n 2) 0))

(print (fast-expt 3 4 1))
(print (fast-expt 2 16 1))
(print (fast-expt 5 3 1))
