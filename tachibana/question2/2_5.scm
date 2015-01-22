(define (ex-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (pair z value result)
  (if (= (remainder z value) 0)
      (pair (/ z value) value (+ result 1))
      result))

(define (ex-car z)
  (pair z 2 0))
(define (ex-cdr z)
  (pair z 3 0))

(print "(ex-cdr (ex-cons 100 5))")
(print (ex-cdr (ex-cons 100 5)))

(print "(ex-car (ex-cons 1000 5))")
(print (ex-car (ex-cons 1000 5)))

