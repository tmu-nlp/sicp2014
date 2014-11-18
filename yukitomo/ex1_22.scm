;ex1_22

(define (square x) (* x x))

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (<= start end)
      (if (even? start)
          (search-for-primes (+ start 1) end) ;when even
          (begin ;when odd
            (timed-prime-test start)
            (search-for-primes (+ start 2) end)))))


;O(root(n))
;O(root(1000)) 
(search-for-primes 1000 1020)
(newline)
(display "-------------------")
;O(root(10000)) = 12 * root(10)=37.9
(search-for-primes 10000 10050)
(newline)
(display "-------------------")
;O(root(100000))= 39 * root(10) = 123
(search-for-primes 100000 100050)
(search-for-primes 1000000 1000100)



 