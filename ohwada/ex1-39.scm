(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))


(define (tan-cf x k)
  (define (square x) (* x x))
  (cont-frac (lambda (i)
               (cond ((= i 1) x)
                     (else (- (square x)))))
             (lambda (i) (+ 1 (* (- i 1) 2)))
             k))

(print (tan-cf 1.0 100)) ; 1.557407... (x = 1 ラジアンの tanx)
             
