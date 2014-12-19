; double
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))


(print (((double (double double)) inc) 5)) ; 21
