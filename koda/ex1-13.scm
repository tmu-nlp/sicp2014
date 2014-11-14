(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (average x y)
  (/ (+ x y) 2.0))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (power x n)
  (cond ((= n 1) x)
    (else (* x (power x (- n 1))))))

(define phi (/ (+ 1 (sqrt-iter 1 5)) 2))
(define psi (/ (- 1 (sqrt-iter 1 5)) 2))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define n 10)

(print (fib n))
(print (/ (- (power phi n) (power psi n)) (sqrt-iter 1 5))) 
