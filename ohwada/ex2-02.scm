(define (make-segment a b)
  (cons a b))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))


(define (make-point x y)
  (cons x y))

(define (x-point x) (car x))
(define (y-point x) (cdr x))


(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;(print-point (make-point 1 2))


(define (midpoint-segment segment)
  (make-point
    (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
    (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(print-point
  (midpoint-segment
    (make-segment (make-point 0 0) (make-point 5 5))))
