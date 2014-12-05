;ex1_41.scm
(define (inc a) (+ a 1))

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

