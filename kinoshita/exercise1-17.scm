(define (times a b)
    (cond ((= b 0) 0)
    ((even? b) (double (times a (halve b))))
    (else (+ a (times a (- b 1))))))

(define (double a) (* a 2))
(define (halve a) (/ a 2))
(define (even? n)
    (= (remainder n 2) 0))

(print (times 2 4))
(print (times 3 5))
