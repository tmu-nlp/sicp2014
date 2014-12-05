(define (filterd-accumulate combiner null-value term a next b filter)
    (define (iter a result)
        (cond 
            ((> a b) result)
            ((filter a) (iter (next a) (combiner (term a) result)))
            (else (iter (next a) result))))
    (iter a null-value))

(define (sum-prime a b)
    (define (inc i) (+ i 1))
    (define (f x) x)
    (filterd-accumulate + 0 f a inc b prime?))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (product-coprime n)
    (define (inc i) (+ i 1))
    (define (f x) x)
    (define (coprime? x)
        (if (= (gcd x n) 1) #t #f))
    (filterd-accumulate * 1 f 1 inc n coprime?))

(print (sum-prime 2 20))
(print (product-coprime 6))
