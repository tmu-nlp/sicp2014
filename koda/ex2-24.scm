(use srfi-27)

(define (count-leaves x)
  (cond ((null? x) 0)
		((not (pair? x)) 1)
		(else (+ (count-leaves (car x))
				 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(print (length x))
(print (count-leaves x))
(print (list x x))
(print (length (list x x)))
(print (count-leaves (list x x)))

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

(define (square-list items)
  (map square items))

(define (square-list items)
  (define (iter things answer)
	(if (null? things)
	  answer
	  (iter (cdr things)
			(cons (square (car things))
				  answer))))
  (iter items ()))

(define (square-list items)
  (define (iter things answer)
	(if (null? things)
	  answer
	  (iter (cdr things)
			(cons answer
				  (square (car things))))))
  (iter items ()))

(print (list 1 (list 2 (list 3 4))))
