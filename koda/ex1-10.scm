(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

(print (A 1 10 ))
(print (A 2 4 ))
(print (A 3 3 ))

(define (f n) (A 0 n))
;=2y
(define (g n) (A 1 n))
;=2^y
(define (h n) (A 2 n))
;=2↑↑y (同じ数のべき乗の繰り返し)
(define (k n) (* 5 n n))
;=5n^2
