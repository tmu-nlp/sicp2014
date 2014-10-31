;; required
(use srfi-19)
(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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

(define (next x)
  (if (= 2 x)
    3
    (+ 2 x)))

(timed-prime-test 1009)
(print "")
(timed-prime-test 1013)
(print "")
(timed-prime-test 1019)
(print "")

(timed-prime-test 10007)
(print "")
(timed-prime-test 10009)
(print "")
(timed-prime-test 10037)
(print "")

(timed-prime-test 1000003)
(print "")
(timed-prime-test 1000037)
(print "")
(timed-prime-test 1000039)
(print "")

