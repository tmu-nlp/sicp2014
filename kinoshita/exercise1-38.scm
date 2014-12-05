(define (cont-frac n d k)
    (define (iter i)
        (if (> i k)
            0
            (/ (n i) (+ (d i) (iter (+ i 1))))))
    (iter 1))
(define (cont-frac2 n d k result)
    (if (= k 0)
        result
        (cont-frac2 n d (- k 1) (/ (n k) (+ (d k) result)))))

(define (d i)
    (if (= (remainder (- i 1) 3) 1)
        (* 2 (+ (/ (- i 2) 3) 1))
        1))

(print (+ 2 (cont-frac2 (lambda (i) 1.0) d 100 0)))
