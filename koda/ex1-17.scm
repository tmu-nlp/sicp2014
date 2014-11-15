(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))

(define (fast-multiple a b)
  (cond ((= a 0) 0)
	((= b 0) 0)
	((even? b) (fast-multiple (double a) (halve b)))
	(else (+ a (fast-multiple a (- b 1))))))
(define (even? n)
  (= (remainder n 2) 0))
   
(print (fast-multiple 2 10))
