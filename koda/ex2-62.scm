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
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
	(caddr p)
	(cons '* (cddr p))))
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
		((=number? exponent 1) base)
		 (else (list '** base exponent))))
(define (augend s)
  (if (null? (cdddr s))
	(caddr s)
	(cons '+ (cddr s))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))
#|
(define (adjoin-set x set)
  (if (element-of-set? x set)
	set
	(cons x set)))
|#
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))
#|
(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((element-of-set? (car set1) set2)
		 (union-set (cdr set1) set2))
		(else (cons (car set1)
					(union-set (cdr set1) set2)))))
|#
(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) set2)))
		(else (intersection (cdr set1) set2))))
(define adjoin-set cons)
(define union-set append)

(define (element-of-set? x set)
  (cond ((null? set) #f)
		((= x (car set)) #t)
		((< x (car set)) #f)
		(else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		(let ((x1 (car set1)) (x2 (car set2)))
	
		  (cond ((= x1 x2)
				 (cons x1 (intersection-set (cdr set1)
											(cdr set2))))
				((< x1 x2)
				 (intersection-set (cdr set1) set2))
				((< x2 x1)
				 (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set1)
		((null? set2) set2)
		(else 
		  (let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2)
				   (cons x1 (union-set (cdr set1) (cdr set2))))
				  ((< x1 x2)
				   (cons x1 (union-set (cdr set1) set2)))
				  ((< x2 x1)
				   (cons x1 (union-set set1 (cdr set2)))))))))

(print (union-set '(3 4) '(3 4 5)))

