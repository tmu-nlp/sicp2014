(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))))
    (iter a 0))

(define (inc i) (+ i 1))
(define (f x) x)
(print (sum f 0 inc 10))
