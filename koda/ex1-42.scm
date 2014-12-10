(use srfi-27)

(define (square x)
  (* x x))
(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(print ((compose square inc) 6))

