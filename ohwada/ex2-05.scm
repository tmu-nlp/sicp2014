(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (iter x n)
    (if (even? x)
        (iter (/ x 2) (+ n 1))
        n))
    (iter z 0))

(define (cdr z)
  (define (iter x n)
    (if (= (remainder x 3) 0)
        (iter (/ x 3) (+ n 1))
        n))
    (iter z 0))



(print (car (cons 8 1)))
(print (cdr (cons 4 13)))
