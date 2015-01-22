(use srfi-27)

(define (make-interval a b) (cons a b))
(define (lower-bound x) (if (< (car x) (cdr x))
						  (car x)
						  (cdr x)))
(define (upper-bound x) (if (> (car x) (cdr x))
						  (car x)
						  (cdr x)))

(define (make-center-percent c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (display (center i))
  (display "±")
  (display (/ (* (width i) 100.) (center i)))
  (display "%")
  (newline)) 

(percent (make-center-percent 10 5))

