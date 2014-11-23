#!/usr/bin/gosh

(define (A x y) 
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))


(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(f 3)
(g 10)
(h 4)


;(f n) = 2n

;(g 4) 
;= (A 1 4)
;= (A (- 1 1) (A 1 (- 4 1)))
;= (A 0 (A 1 (- 4 1)))
;= (* 2 (A 1 (- 4 1)))
;= (* 2 (A 1 3))
;= (* 2 (A 0 (A 1 (- 3 1))))
;= (* 2 (* 2 (A 1 (- 3 1))))
;= (* 2 (* 2 (A 1 2)))
;= (* 2 (* 2 (A 0 (A 1 (- 2 1)))))
;= (* 2 (* 2 (* 2 (A 1 (- 2 1)))))
;= (* 2 (* 2 (* 2 (A 1 1))))
;= (* 2 (* 2 (* 2 2)))
;= 2^4


;(g n) 
;= (A 1 n)
;= (A (- 1 1) (A 1 (- n 1)))
;= (A 0 (A 1 (- n 1)))
;= (* 2 (A 1 (- n 1)))
;= (* 2 (* 2 ((A 1 (- n 2)))))
;= (* 2 (* 2 (* 2 (- n 3))))
;= (* 2 (* 2 (* 2 (* 2 (- n 4)))))
;= (* 2 (* 2 (* 2 (* 2 (* 2 (- n 5))))))
;= (* 2 (* 2 (* 2 (* 2 ... 2))))
;= 2^n
;
;(h n)
;= (A 2 n)
;= (A (- 2 1) (A 2 (- n 1)))
;= (A 1 (A 2 (- n 1)))
;= (A 1 (A 1 (A 2 (- n 2))))
;= (A 1 (A 1 (A 1 (A 2 (- n 3))))
;= (A 1 (A 1 (A 1 (A 1 (A 2 (- n 4))))))
;= (A 1 (A 1 (A 1 (A 1 (A 1 (A 2 (- y 5)))))))
;= (A 1 (A 1 (A 1 (A 1 (A 1 ... (A 2 0))))))
;= (A 1 (A 1 (A 1 (A 1 (A 1 ... 0)))))
;= (g (g (g (g .....(g 1)))))
;= 2^2^2^2^...=2↑↑n
;F_n = (F_{n-1})^2