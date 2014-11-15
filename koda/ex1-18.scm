(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))

(define (alter-multiple a b)
  (alter-multiple-iter a b 0))
(define (alter-multiple-iter a b x)
  (cond ((= b 0) x)
	((even? b) (alter-multiple-iter (double a) (halve b) x ))
	(else (alter-multiple-iter (double a) (halve (- b 1)) (+ x a)))))
  
(print (alter-multiple 20 10))


