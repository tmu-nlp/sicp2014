(define (timed-prime-test n)
    (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
    (display "***") (display elapsed-time))
(define (prime? n) (= n (smallest-divisor n)))

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

(define (even? n) (= (remainder n 2) 0))

(define (search-for-primes a b)
    (cond ((< b a) #f)
          ((even? a) (search-for-primes (+ a 1) b))
          (else
            (begin (timed-prime-test a)
                (search-for-primes (+ a 2) b)))))

(timed-prime-test 1000)
(search-for-primes 1000 1020)
;1009,1013,1019
;6micro sec
(search-for-primes 10000 10038)
;10007,10009,10037
;20micro sec
(search-for-primes 100000 100044)
;100003,100019,100043
;60micro sec
(search-for-primes 1000000 1000038)
;1000003,1000033,1000037
;140micro sec
;おおよそsqrt(10)倍ずつになっている
