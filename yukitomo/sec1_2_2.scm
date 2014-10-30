;sec1_2_2.scm

(define (fib_r n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib_r (- n 1))
                 (fib_r (- n 2))))))



(define (fib_i n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;(fib_i 4)
;(fib-iter 1 0 4)
;(fib-iter 1 1 3)
;(fib-iter 2 1 2)
;(fib-iter 3 2 1)
;(fib-iter 5 3 0)
;3