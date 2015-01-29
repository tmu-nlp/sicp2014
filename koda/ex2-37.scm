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

(print v)
(print w)
(print m)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(print (dot-product v w))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(print (matrix-*-vector m v))

(define (transpose mat)
  (accumulate-n cons () mat))
(print (transpose m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
	(map (lambda (x) (matrix-*-vector cols x)) m)))
(print (matrix-*-matrix m m))

;32
;(14 32 44)
;((1 4 6) (2 5 7) (3 6 8))
;((27 33 39) (60 75 90) (82 103 124))

