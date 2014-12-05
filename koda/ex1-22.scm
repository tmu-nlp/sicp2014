(define (runtime)
    (- (time->seconds (current-time)) 1136041200))
(define (square x)
  (* x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) (+ test-divisor 1))
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
  (if (> b 0)
	(if (and (prime? a)
			  (timed-prime-test a))
	   (search-for-primes (+ a 1) (- b 1))
	   (search-for-primes (+ a 1) b))))

(print (search-for-primes 1000 3))
(print (search-for-primes 10000 3))
(print (search-for-primes 100000 3))
(print (search-for-primes 1000000 3))

;1009 *** 1.1920928955078125e-5
;1013 *** 1.1920928955078125e-5
;1019 *** 1.4066696166992187e-5#<undef>
;
;10007 *** 3.3855438232421875e-5
;10009 *** 3.4809112548828125e-5
;10037 *** 3.504753112792969e-5#<undef>
;
;100003 *** 7.700920104980469e-5
;100019 *** 7.891654968261719e-5
;100043 *** 7.510185241699219e-5#<undef>
;
;1000003 *** 2.009868621826172e-4
;1000033 *** 2.0194053649902344e-4
;1000037 *** 2.009868621826172e-4#<undef>
