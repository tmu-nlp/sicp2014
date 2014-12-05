;ex1_43.scm

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define i 0)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)