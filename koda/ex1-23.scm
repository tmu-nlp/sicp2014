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

;1009 *** 1.2874603271484375e-5#<undef>
;1013 *** 1.1920928955078125e-5#<undef>
;1019 *** 1.3113021850585937e-5#<undef>
;
;10007 *** 3.695487976074219e-5#<undef>
;10009 *** 2.3126602172851562e-5#<undef>
;10037 *** 3.600120544433594e-5#<undef>
;
;100003 *** 9.894371032714844e-5#<undef>
;100019 *** 1.0204315185546875e-4#<undef>
;100043 *** 6.008148193359375e-5#<undef>
;
;1000003 *** 1.8715858459472656e-4#<undef>
;1000033 *** 1.8715858459472656e-4#<undef>
;1000037 *** 1.8715858459472656e-4#<undef>
