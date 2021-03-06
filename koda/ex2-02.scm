(use srfi-27)

(define (average x y)
  (/ (+ x y) 2.0))

(define (make-segment x1 y1 x2 y2)
  (cons (cons x1 y1) (cons x2 y2)))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (midpoint-segment segment)
  (cons (average (x-point (start-segment segment))
				 (x-point (end-segment segment)))
		(average (y-point (start-segment segment))
				 (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define sample-segment (make-segment 2 3 1 5))
(print-point (start-segment sample-segment)) 
(print-point (end-segment sample-segment)) 
(print-point (midpoint-segment sample-segment)) 
