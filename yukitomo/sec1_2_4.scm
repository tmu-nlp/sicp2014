;sec1_2_4.scm
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 
;(expt 5 3)
;(expt-iter 5 3 1)
;(expt-iter 5 2 5)
;(expt-iter 5 1 25)
;(expt-iter 5 0 125)
;125

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))