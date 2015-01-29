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

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

(define s (accumulate
			append () (map (lambda (i)
							 (map (lambda (j) (list i j))))
								  (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (flatmap
				 (lambda (i)
				   (map (lambda (j) (list i j))
						(enumerate-interval 1 (- i 1))))
				 (enumerate-interval 1 n)))))

(define (permutation s)
  (if (null? s)
	(list ())
	(flatmap (lambda (x)
			   (map (lambda (p) (cons x p))
					(permutations (remove x s))))
			 s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
		  sequence))

(define (unique-triples n)
  (flatmap
	(lambda (i)
	  (flatmap
		(lambda (j)
		  (map (lambda (k) (list i j k))
			   (enumerate-interval i (- j 1))))
		(enumerate-interval 1 (- i 1))))
	(enumerate-interval 1 n)))

(print (unique-triples 4))
