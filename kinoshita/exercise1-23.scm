(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (next n)
    (if (= n 2) 3 (+ n 2)))
(define (timed-prime-test n)
    (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
    (display "***") (display elapsed-time))
(define (prime? n) (smallest-divisor n))
;gaucheにはruntimeがないので実装
(define (runtime)
    (use srfi-11)
        (let-values (((a b) (sys-gettimeofday)))
        (+ (* a 1000000) b)))

(define (even? n) (remainder n 2))

(define (search-for-primes a b)
    (cond ((< b a) #f))
        ((even? a) (search-for-primes (+ a 1) b))
        (else
            (begin (timed-prime-test a)
            (search-for-primes (+ a 2) b))))

(print (smallest-divisor 199))
;199
(print (smallest-divisor 1999))
;1999
(print (smallest-divisor 19999))
;7
