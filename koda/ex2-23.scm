(use srfi-27)

(define (scale-list items factor)
  (if (null? items)
	()
	(cons (* (car items) factor)
		  (scale-list (cdr items)
					  factor))))

(define (map proc items)
  (if (null? items)
	()
	(cons (proc (car items))
		  (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
	   items))

(for-each (lambda (x)
			(newline)
			(display x))
		  (list 57 321 88))

(define (for-each proc items)
  (if (null? items)
	()
	(and (proc (car items))
		 (for-each proc (cdr items)))))

(for-each (lambda (x)
			(newline)
			(display x))
		  (list 57 321 88))
