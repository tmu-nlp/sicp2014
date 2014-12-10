(use srfi-27)
(define (expmod base exp m)
  (cond ((= exp 0)
		 1)
		((even? exp)
		 (remainder
		   (square
			 (expmod base (/ exp 2) m))
		   m))
		(else
		  (remainder
			(* base
			   (expmod base (- exp 1) m))
			m))))
(define (random num)
    (* (random-integer num) 1.0))
(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

(define (next x)
  (cond ((= x 2) 3)
		(else (+ x 2))))
(define (runtime)
    (- (time->seconds (current-time)) 1136041200))
(define (square x)
  (* x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) (next test-divisor))
		(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
	(report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

(define (search-for-primes a b)
  (if (< a b)
	(and
	  (if (fast-prime? a 100)
		(timed-prime-test a))
	  (search-for-primes (+ a 1) b))))

;未完
