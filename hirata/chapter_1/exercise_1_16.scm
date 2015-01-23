#!/usr/local/bin/gosh

(define (even? n)
  (= (remainder n 2) 0))

(define (fast_expt2 b n a)
  (cond ((= n 0) a)
	((even? n) (fast_expt2 (* b b) (/ n 2) a))
    (else (fast_expt2 b (- n 1) (* a b))))
  )

(print (fast_expt2 2 3 1))
