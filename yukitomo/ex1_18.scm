;ex1_8.rkt
(define (double x) (* x 2))
(define (halve x) (/ x 2))
(define (even? n) (= (remainder n 2) 0))

(define (multiply-rep x y)
  (define (itr x y n)
    (cond ((= y 0) n)
          ((even? y) (itr (double x) (halve y) n))
          (else (itr x (- y 1) (+ n x)))))
  (itr x y 0))


(multiply-rep 4 4)
;(itr 4 4 0)
;(itr 8 2 0)
;(itr 16 1 0)

(multiply-rep 4 5)
;(itr 4 5 0)
;(itr 4 4 4)
;(itr 8 2 4)
;(itr 16 1 4)
;(itr 16 0 20)
;20
