(use srfi-27)

(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 1 0))
		((sum? exp) (make-sum (deriv (append exp) var)
							  (deriv (augend exp) var)))
		((product? exp)
		 (make-sum
		   (make-product (multiplier exp)
						 (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var)
						 (multiplicand exp))))
	
		((exponentiation? exp)
		 (make-product (exponent exp)
					   (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
									 (deriv (base exp) var))))
		(else (display "unknown expression type: DERIV"))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2))
		 (+ a1 a2))
		(else (list '+ a1 a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
		((=number? exponent 1) base)
		 (else (list '** base exponent))))

(print (deriv '(* x y (+ x 3)) 'x))
(print (deriv '(** x 0) 'x))
(print (deriv '(** x 1) 'x))
(print (deriv '(** x 2) 'x))
(print (deriv '(** x 3) 'x))
