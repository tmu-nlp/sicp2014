;ex1_16

;recursive
(define (fast-expt-r b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-r b (/ n 2))))
        (else (* b (fast-expt-r b (- n 1))))))

(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))

(fast-expt-r 2 4)
;(square (fast-expt-r 2 2))
;(square (sqaure (square (fast-expt-r 2 1))))
;(square (sqaure (square (* 2 (fast-expt 2 0)))))

;iterative
(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((= n 1) (* b a))
        (else (fast-expt-iter b (- n 2) (* (square b) a))))) 

(fast-expt-i 2 4)
;(fast-expt-iter 2 4 1)
;(fast-expt-iter 2 2 4)
;(fast-expt-iter 2 0 16)