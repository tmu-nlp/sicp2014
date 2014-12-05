(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
            (term a)
            (accumulate combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(define (sum term a next b)
    (accumulate + 0 term a next b))
(define (product term a next b)
    (accumulate2 * 1 term a next b))

(define (inc i) (+ i 1))
(define (f x) x)

(print (sum f 0 inc 10))
(print (product f 1 inc 5))
