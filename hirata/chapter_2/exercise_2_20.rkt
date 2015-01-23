(define (odd? val)
  (= 1 (remainder val 2)))

(define (even? val)
  (= 0 (remainder val 2)))

(define (even-list list)
  (cond ((null? list) '())
		((even? (car list)) (cons (car list) (even-list (cdr list))))
		(else (even-list (cdr list)))))

(define (odd-list list)
  (cond ((null? list) '())
		((odd? (car list)) (cons (car list) (odd-list (cdr list))))
		(else (odd-list (cdr list)))))

(define (same-parity . args)
  (if (odd? (car args)) (odd-list args)
	  (even-list args)))

(display (same-parity 1 2 3 4 5 6 7)) ;=> (1 3 5 7)
(newline)
(display (same-parity 2 3 4 5 6 7)) ;=> (2 4 6)