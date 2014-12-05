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

(define (iter-k k n)
    (if (> k n)
    0
    (begin
        (display
            (cont-frac  (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k))
        (newline)
        (iter-k (+ k 1) n))))

(iter-k 1 100)
;11回

(print (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 20 0))
