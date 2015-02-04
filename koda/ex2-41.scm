(use srfi-27)

(define (square x)
  (* x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (enumerate-interval low high)
  (if (> low high)
	()
	(cons low (enumerate-interval (+ low 1) high))))

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

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
		  sequence))

(define (unique-pairs n)
  (flatmap (lambda (i)
			 (map (lambda (j) (list i j))
				  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))
(print (unique-pairs 3))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (unique-pairs n))))
(print (prime-sum-pairs 3))

(define (unique-triples n)
  (flatmap (lambda (i)
			 (flatmap (lambda (j)
					(map (lambda (k) (list i j k))
						 (enumerate-interval 1 (- j 1))))
				  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))

(define (same-sum-triples n s)
  (define (same-sum? triple)
	(= (sum triple) s))
  (filter same-sum?
		  (unique-triples n)))

(define (sum triple)
  (+ (car triple) (cadr triple) (caddr triple)))

(print (same-sum-triples 10 10))
