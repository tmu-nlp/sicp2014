; 以下素数を求める手続き

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))


(define (prime? n)
  (= n (smallest-divisor n)))


; next 手続き

(define (next n)
  (if (= n 2) 3
      (+ n 2)))

;---------------------------------------------
; 以下 timed-prime-test 手続き

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (display " *** ")
  (display elapsed-time))

; gauche には runtime がないので以下の手続きを定義

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

;-----------------------------------------------
; 以下 search-for-primes 手続き

(define (search-for-primes n c)
  (cond ((= c 3) (newline) (display " fin ") (newline))
        ((even? n) (search-for-primes (+ n 1) c))
        ((prime? n) (timed-prime-test n) (search-for-primes (+ n 2) (+ c 1)))
        (else (search-for-primes (+ n 2) c))))

(define (even? n)
  (= (remainder n 2) 0))



; テスト

(search-for-primes 1000 0)
(search-for-primes 10000 0)
(search-for-primes 100000 0)
(search-for-primes 1000000 0)
(search-for-primes 10000000 0)
