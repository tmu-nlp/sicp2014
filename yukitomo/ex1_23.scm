;ex1_23

;Primes found in ex1_22.scm
;1009, 1013, 1019
;10007, 10009, 10037
;100003, 100019, 100043

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;------------------------------------------------------

(define (fast-smallest-divisor n)
  (fast-find-divisor n 2))

(define (fast-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ;test-devisor = 2 > 2 + 1 , test-devisor = 3 > 3 + 2 
        ;skip even numbers 
        (else (find-divisor n (next test-divisor))))) 

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
        (+ n 1)
        (+ n 2)))

(fast-smallest-divisor 1009)
(fast-smallest-divisor 1013)
(fast-smallest-divisor 1019)

