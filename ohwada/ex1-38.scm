(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(print
  (+ (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (cond ((= (remainder (+ i 1) 3) 0) (* (/ (+ i 1) 3) 2))
                        (else 1.0)))
                10)
      2)) ; 2.718281... 
