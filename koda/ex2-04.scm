(use srfi-27)

(define (average x y)
  (/ (+ x y) 2.0))

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define pair (cons 2 3)) 
(print (car pair)) 
(print (cdr pair)) 
