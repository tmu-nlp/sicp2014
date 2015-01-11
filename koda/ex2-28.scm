(use srfi-27)

(define (count-leaves x)
  (cond ((null? x) 0)
		((not (pair? x)) 1)
		(else (+ (count-leaves (car x))
				 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

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

(define (reverse items)
  (define (reverse-iter items result)
	(if (null? items)
	  result 
	  (reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items ()))

(define (deep-reverse items)
  (define (reverse-iter items result)
	(if (null? items)
	  result 
	  (reverse-iter (cdr items) (cons (reverse (car items)) result))))
  (reverse-iter items ()))

(define (fringe items)
  (cond ((null? items) items)
		((not (pair? items)) (list items)) 
		(else (append (fringe (car items)) (fringe (cdr items))))))

(define x (list (list 1 2) (list 3 4)))
(print x)
(print (fringe x))
(print (fringe (list x x)))

