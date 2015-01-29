(use srfi-27)

(define (square x)
  (* x x))

(define (scale-tree tree factor)
  (cond ((null? tree) ())
		((not (pair? tree)) (* tree factor))
		(else (cons (scale-tree (car tree) factor)
					(scale-tree (cdr tree) factor)))))
(print (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		   (scale-tree sub-tree factor)
		   (* sub-tree factor)))
	   tree))

(print (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))

(define (square-tree tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		   (square-tree sub-tree)
		   (square sub-tree)))
	   tree))

(print (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) ))


