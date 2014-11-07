;ex_1_17.rkt

;O(b)
;(define (multi-ply a b)
;  (if (= b 0)
;     0
;      (+ a (multi-ply a (- b 1)))))
;(+ a (+ a (+ a ...(+ a (+ a 0) 


;xy = 2x * (y/2)
;xy = x+(y-1)x
(define (double x) (* 2 x))
(define (halve x) (/ x 2))
(define (even? n) (= (remainder n 2) 0))

(define (multiply-rec x y)
  (cond ((= y 1) x)
        ((even? y) (multiply-rec (double x) (halve y)))
        (else (+ x (multiply-rec (- y 1) x)))))

(multiply-rec 4 4)
;(multiply-rec 8 2)
;(multiply-rec 16 1)
;16 

(multiply-rec 4 5)
;(+4 (multiply-rec 4 4))
;(+4 (multiply-rec 8 2))
;(+4 (multiply-rec 16 1))
;(+4 16)
;20