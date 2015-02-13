(use srfi-27)

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
					   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
					   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
					 (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					 (- (angle z1) (angle z2))))

;Ben
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
		   (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
;Alyssa
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y) 
  (cons (sqrt (+ (square x) (square y)))
		(atan y x)))
(define (make-from-mag-ang r a) (cons r a))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
	(car datum)
	(display "Bad tagged datum: TYPE-TAG")))
(define (contents datum)
  (if (pair? datum)
	(cdr datum)
	(display "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

(define (make-table)
  (let ((local-table (list '*table*)))
	(define (lookup key-1 key-2)
	  (let ((subtable
			  (assoc key-1 (cdr local-table))))
		(if subtable
		  (let ((record
				  (assoc key-2 (cdr subtable))))
			(if record (cdr record) #f))
		  #f)))
	(define (insert! key-1 key-2 value)
	  (let ((subtable
			  (assoc key-1 (cdr local-table))))
		(if subtable
		  (let ((record
				  (assoc key-2 (cdr subtable))))
			(if record
			  (set-cdr! record value)
			  (set-cdr! subtable
						(cons (cons key-2 value)
							  (cdr subtable)))))
		  (set-cdr! local-table
					(cons (list key-1 (cons key-2 value))
						  (cdr local-table)))))
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			(else (display "Unknown operation: TABLE"))))
  dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		(else ((get 'deriv (operator exp))
			   (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(print (deriv '(+ x 3) 'x))
