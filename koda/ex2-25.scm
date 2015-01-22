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

(define list25-1 (list 1 3 (list 5 7) 9))
(define list25-2 (list (list 7)))
(define list25-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(print list25-1)
(print list25-2)
(print list25-3)

(print (car (cdr (car (cdr (cdr list25-1))))))
(print (car (car list25-2)))
(print (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list25-3)))))))))))))

