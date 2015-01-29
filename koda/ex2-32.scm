(use srfi-27)

(define (subsets s)
  (if (null? s)
	(list ())
	(let ((rest (subsets (cdr s))))
	  (append rest (map (lambda (element)
						  (cons (car s) element))
						  rest)))))

(print (subsets (list 1 2 3)))
