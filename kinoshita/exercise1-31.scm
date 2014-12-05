(define (even? x)
    (= (remainder x 2) 0))

(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
            (product term (next a) next b))))

(define (product2 term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (inc i) (+ i 1))
(define (f x) x)

(print (product f 1 inc 5))
(print (product2 f 1 inc 5))

(define (square x) (* x x))
(define (calc-pi n)
    (define (next a)
        (+ a 2))
    (define product-even
        (* 2 n (square (product2 f 4 next (- n 2)))))
    (define product-odd
        (square (product2 f 3 next (- n 1))))
    (* 4.0 (/ product-even product-odd)))

(print (calc-pi 100))
(print (calc-pi 1000))
(print (calc-pi 10000))
