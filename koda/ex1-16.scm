(define (square x)
  (* x x))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
	       (- counter 1)
	       (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

(define (alter-expt b n)
  (alter-expt-iter 1 b n))

(define (alter-expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (alter-expt-iter a (square b) (/ n 2)))
	(else (alter-expt-iter (* a b) b (- n 1)))))

(print (fast-expt 2 10))
(print (alter-expt 2 10))

