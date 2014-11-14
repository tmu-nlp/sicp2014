(define (even? n)
  (= (remainder n 2) 0))
(define (square x)
  (* x x))
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square q) (square p));compute p'
		   (+ (square q) (* 2 p q));compute q'
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b q) (* a q))
			p
			q
			(- count 1)))))

(define (re-print x) 
  (cond ((<= x 10)
	 ;(print x)
	 (print (fib x))
	 (re-print (+ x 1)))))
(re-print 1)


