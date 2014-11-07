(define (square x)
  (* x x))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n)
          (fast-expt-iter a (square b) (/ n 2)))
        (else
          (fast-expt-iter (* a b) b (- n 1)))))

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(fast-expt 2 8)
;; => 256