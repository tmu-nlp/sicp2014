; #lang scheme

;
(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

; fast-expt
(define (fast-expt b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* b a)))))
  (iter b n 1))

;    b        ^  n
; = (b^2)     ^ (n/2)
; =  NEW b    ^  NEW n

; run
(fast-expt 2 10)

