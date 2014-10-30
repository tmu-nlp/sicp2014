(define (even? n)
  (= (modulo n 2) 0))

(define (fast-expt b n)
  (define a 1)
  ; a*b^n
  (define (fast-expt-itr a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-itr a (* b b) (/ n 2)))
          (else (fast-expt-itr (* a b) b (- n 1)))))
  (fast-expt-itr a b n))
