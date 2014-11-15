(use srfi-27)
(define true #t)
(define false #f)
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
(define (next x)
  (cond ((= x 2) 3)
		(else (+ x 2))))
(define (runtime)
    (- (time->seconds (current-time)) 1136041200))
(define (square x)
  (* x x))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) (next test-divisor))
		(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
;(define (prime? n)
;  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
	(report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

(print (timed-prime-test 1009)) 
(print (timed-prime-test 1013)) 
(print (timed-prime-test 1019))

(print (timed-prime-test 10007)) 
(print (timed-prime-test 10009)) 
(print (timed-prime-test 10037))

(print (timed-prime-test 100003))
(print (timed-prime-test 100019))
(print (timed-prime-test 100043))

(print (timed-prime-test 1000003))
(print (timed-prime-test 1000033))
(print (timed-prime-test 1000037))

;1009 *** 6.699562072753906e-5#<undef>
;1013 *** 5.91278076171875e-5#<undef>
;1019 *** 6.604194641113281e-5#<undef>
;
;10007 *** 6.723403930664062e-5#<undef>
;10009 *** 6.29425048828125e-5#<undef>
;10037 *** 6.604194641113281e-5#<undef>
;
;100003 *** 7.295608520507812e-5#<undef>
;100019 *** 7.581710815429687e-5#<undef>
;100043 *** 7.700920104980469e-5#<undef>
;
;1000003 *** 8.511543273925781e-5#<undef>
;1000033 *** 8.606910705566406e-5#<undef>
;1000037 *** 8.511543273925781e-5#<undef>
