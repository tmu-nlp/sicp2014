;#lang racket
;;;;;;1_21.scm
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;(define (prime? n)
;  (= n (smallest-divisor n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (next+ n)
  (cond ((even? n) (+ n 1))
        (else (+ n 2))))

(define (next- n)
  (cond ((even? n) (- n 1))
        (else (- n 2))))

(define (search-for-prime-large n m)
  (cond ((> m 0)
         (cond ((fast-prime? n 100) 
                (newline)
                (display n)
                (search-for-prime-large (next+ n) (- m 1)))
               (else
                 (search-for-prime-large (next+ n) m))))))

(define (search-for-prime-small n m)
  (cond ((> m 0)
         (cond ((fast-prime? n 1000) 
                (newline)
                (display n)
                (search-for-prime-small (next- n) (- m 1)))
               (else
                 (search-for-prime-small (next- n) m))))))

(define (search-for-prime n m start-time)
  (search-for-prime-large n m)
  (search-for-prime-small n m)
  (newline)
  (display (- (real-time-clock) start-time)))
;(timed-prime-test 10007)
;(search-for-prime 1000 3 (real-time-clock))
;(search-for-prime 10000 3 (real-time-clock))
(search-for-prime 100000000000 3 (real-time-clock))
;6640ms

