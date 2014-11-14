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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (random x)
  (modulo (sys-random) x))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (try-it-iter b)
    (if (= b 1)
      (try-it b)
      (and (try-it b) (try-it-iter (- b 1)))))
  (try-it-iter (- n 1)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; required


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
    (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


;; my script

; carmichael number
; 561, 1105, 1729, 2465, 2821, 6601

(print 561)
(print (fermat-test 561))


(print 1105)
(print (fermat-test 1105))


(print 1729)
(print (fermat-test 1729))
