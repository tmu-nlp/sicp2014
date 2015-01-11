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

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
;bの最中
(define (total-weight mobile)
  (define (weight branch result)
	(cond ((null? branch) 0)
		  ((not (pair? branch)) result) 
		  (else (+ (weight (left-branch branch) result)
				   (weight (right-branch branch) result)))))

  (weight mobile 0))

(define list25-1 (list 1 3 (list 5 7) 9))
(define list25-2 (list (list 7)))
(define list25-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))



(print (make-mobile list25-1 list25-2))
(print (left-branch (make-mobile list25-1 list25-2)))
(print (right-branch (make-mobile list25-1 list25-2)))
(print (total-weight (make-mobile list25-1 list25-2)))

