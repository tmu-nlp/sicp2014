(use srfi-27)

(define (square x)
  (* x x))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	()
	(cons (accumulate op init (map car seqs))
		  (accumulate-n op init (map cdr seqs)))))

(define v (list 1 2 3))
(define w (list 4 5 6))
(define m (list (list 1 2 3)
				(list 4 5 6)
				(list 6 7 8)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
	(map (lambda (x) (matrix-*-vector cols x)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest)
	  result
	  (iter (op result (car rest))
			(cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
	(if (null? rest)
	  result 
	  (op (car rest) (iter result (cdr rest)))))
  (iter initial sequence))

(print (fold-right / 1 (list 1 2 3)))
(print (fold-left / 1 (list 1 2 3)))
(print (fold-right list () (list 1 2 3)))
(print (fold-left list () (list 1 2 3)))

;交換法則,(= (op a b) (op b a))が成り立つこと

