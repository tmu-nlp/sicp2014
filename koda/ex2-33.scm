(use srfi-27)

(define (square x)
  (* x x))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
		((not (pair? tree))
		 (if (odd? tree) (square tree) 0))
		(else (+ (sum-odd-squares (car tree)))
			  (+ (sum-odd-squares (car tree))))))

(define (even-fibs n)
  (define (next k)
	(if (> k n)
	  ()
	  (let ((f (fib k)))
		(if (even? f)
		  (cons f (next (+ k 1)))
		  (next (+ k 1)))))))

(define (filter predicate sequence)
  (cond ((null? sequence) ())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(print (filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))
(print (accumulate + 0 (list 1 2 3 4 5)))
(print (accumulate * 1 (list 1 2 3 4 5)))
(print (accumulate cons () (list 1 2 3 4 5)))

(define (enumerate-interval low high)
  (if (> low high)
	()
	(cons low (enumerate-interval (+ low 1) high))))
(print (enumerate-interval 2 7))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))
(print (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

(define (sum-odd-squares tree)
  (accumulate 
	+ 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
	cons
	()
	(filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
	cons
	()
	(map square (map fib (enumerate-interval 0 n)))))
(print (list-fib-squares 10))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))
(print (product-of-squares-of-odd-elements (list 1 2 3 4 5)))

(define (salary-of-highest-paid-programmer records)
  (accumulate
	max 0 (map salary (filter programmer? records))))
	
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(print (map square (list 1 2 3 4 5)))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(print (append (list 1 2 3 4 5) (list 6 7 8)))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(print (length (list 1 2 3 4 5)))


