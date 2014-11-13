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

(define (alter-multiple a b x)
  (cond ((= b 0) x)
	((even? b) (alter-multiple (double a) (halve b) x ))
	(else (alter-multiple (double a) (halve (- b 1)) (+ x a)))))
  
(print (alter-multiple 20 10 0))


