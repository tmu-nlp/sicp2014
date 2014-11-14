(define (timed-prime-test n)
    (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
    (display "***") (display elapsed-time))
(define (prime? n) (smallest-divisor n))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

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
(search-for-primes 1000 1050)
(search-for-primes 10000 10050)
(search-for-primes 100000 100050)
