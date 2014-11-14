(define (times a b c)
    (cond ((= b 1) c)
    ((even? b) (times a (halve b) (+ c (double a))))
    (else (+ a (times a (- b 1) c)))))

(define (double a) (* a 2))
(define (halve a) (/ a 2))
(define (even? n)
    (= (remainder n 2) 0))

(print (times 2 4 0))
(print (times 3 5 0))
