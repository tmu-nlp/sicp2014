; fast-prime 手続き

(use srfi-27)
(use srfi-11)

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
  (try-it (+ 1 (random-integer (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))

;--------------------------------------
; timed-prime-test 手続き

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (display " *** ")
  (display elapsed-time))


(define (runtime)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))



; テスト

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(timed-prime-test 10000019)
(timed-prime-test 10000079)
(timed-prime-test 10000103)
(timed-prime-test 100000000003)
(timed-prime-test 10000000000037)
(newline)
