(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (define (iter num count)
    (if (not (=  (remainder num 2) 0))
      count
      (iter (/ num 2) (+ count 1))))
  (iter x 0))


(define (cdr x)
  (define (iter num count)
    (if (not (= (remainder num 3) 0))
      count
      (iter (/ num 3) (+ count 1))))
  (iter x 0))


(define pair (cons 3 5))
(print (car pair))
(print (cdr pair))

