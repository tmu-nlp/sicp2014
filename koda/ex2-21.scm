(use srfi-27)

(define (square x)
  (* x x))

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

(define (square-list items)
  (if (null? items)
	()
	(cons (square (car items)) (square-list (cdr items)))))
(print (square-list (list 1 2 3 4)))

(define (square-list items)
  (map square items))
(print (square-list (list 1 2 3 4)))

