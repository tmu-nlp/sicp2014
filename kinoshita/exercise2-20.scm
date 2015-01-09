(define (even? n)
    (if (= (remainder n 2) 0)
        1
        0))

(define (same-parity n . l)
    (define (iter l2 result)
        (if (null? l2)
            result
            (if (= (even? n) (even? (car l2)))
                (cons (car l2) (iter (cdr l2) result))
                (iter (cdr l2) result))))
    (iter (cons n l) ()))

(print (same-parity 1 2 3 4 5 6 7))
(print (same-parity 2 3 4 5 6 7))
