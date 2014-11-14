#!/usr/bin/gosh


(define (square x) (* x x))
(define (even? x) (= 0 (remainder x 2)))

;テキストのアルゴリズム
;n = 0 の時,  b^0 = 1
;n が偶数の時,  (b^n/2) * (b^n/2) = b^n
;n が奇数の時,  b * b^{n-1} = b^n
(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))


;n = 0 の時,  a * b^0 = 1
;n が偶数の時,  a * (b^2)^{n/2} = a * b^n
;n が奇数の時,  ab * b^{n-1} = a * b^n
(define (fast-expt2 a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt2 a (square b) (/ n 2)))
        (else (fast-expt2 (* a b) b (- n 1)))))

(define (fast-expt b n)
  (fast-expt2 1 b n))


#?=(fast-expt 4 2)
;(fast-expt 2 8)
;(fast-expt2 1 2 8)
;(fast-expt2 1 (square 2) (/ 8 2))
;(fast-expt2 1 4 4)
;(fast-expt2 1 (square 4) (/ 4 2))
;(fast-expt2 1 16 2)
;(fast-expt2 1 (square 16) (/ 2 2))
;(fast-expt2 1 256 1)
;(fast-expt2 (* 1 256) 256 (- 1 1)))
;(fast-expt2 256 256 0))
