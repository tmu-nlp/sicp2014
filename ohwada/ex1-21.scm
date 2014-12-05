; 最小の除数を見つける手続き

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))


(print (smallest-divisor 199))   ; 199
(print (smallest-divisor 1999))  ; 1999
(print (smallest-divisor 19999)) ; 7
