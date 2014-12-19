(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a ) next b))))

(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b)
	   dx))

(define (simpson f a b n)
	(define h (/ (- b a) n))
	(define (yk k)
		(define m
			(cond ((= k 0) 1)
				  ((= k n) 1)
				  ((even? k) 2)
				  (else 4)))
		(* m (f (+ a (* k h)))))
	(* (/ h 3) (sum yk 0 inc n)))

(display (simpson cube 0 1 100))