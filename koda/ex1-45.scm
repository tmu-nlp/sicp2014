(use srfi-27)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2.0))
(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2))
	   tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx))(g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (repeat-root n x k)
  (define (root n x)
	(average-damp (lambda (y) (/ x (expt y (- n 1))))))
  (fixed-point (repeated (root n x) k) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
	f
	(compose f (repeated f (- n 1)))))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(print (repeat-root 2 16 1) ) 
(print (repeat-root 3 27 1) ) 
(print (repeat-root 4 16 2) ) 
(print (repeat-root 256 10 5) ) 

