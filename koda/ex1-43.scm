(use srfi-27)

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1)
	f
	(compose f (repeated f (- n 1)))))

(print ((repeated square 3) 5))

