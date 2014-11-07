(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b count product)
  (cond ((= count 1) product)
        ((even? count) 
            (fast-expt-iter b (/ count 2) (* (square b) product)))
         (else (fast-expt-iter b (- count 1) (* b product)))))
;test
(fast-expt 2 5)

