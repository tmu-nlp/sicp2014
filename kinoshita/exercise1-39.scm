(define (cont-frac n d k)
    (define (iter i)
        (if (> i k)
            0
            (/ (n i) (+ (d i) (iter (+ i 1))))))
    (iter 1))
(define (cont-frac2 n d k result)
    (if (= k 0)
        result
        (cont-frac2 n d (- k 1) (/ (n k) (+ (d k) (- 0 result))))))

(define (tan-cf x k)
    (define (n x)
        (lambda (i)
            (if (= i 1)
                x
                (* x x))))
    (define (d i)
        (- (* i 2) 1))
    (cont-frac2 (n x) d k 0))

(print (tan-cf 0.0 100))
(print (tan-cf 0.5 100))

