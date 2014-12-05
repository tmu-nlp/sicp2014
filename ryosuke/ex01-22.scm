;; required
(use srfi-19)
(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x)
  (* x x))

;; required


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


;; my script

(define (search-for-primes i max)
  (cond ((even? i) (search-for-primes (+ i 1) max))
        ((> i max) #f)
        (else
          (begin
            (timed-prime-test i)
            (search-for-primes (+ i 2) max))))) 

(search-for-primes 1000 1020)
(print "")
(search-for-primes 10000 10040)
(print "")
(search-for-primes 100000 100040)
(print "")
(search-for-primes 1000000 1000050)
(print "")

