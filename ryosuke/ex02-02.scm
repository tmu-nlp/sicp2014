(define (make-point x y)
  (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (midpoint-segment x)
  (cons (/ (+ (x-point (start-segment x)) (x-point (end-segment x))) 2)
        (/ (+ (y-point (start-segment x)) (y-point (end-segment x))) 2)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define line (make-segment (make-point 2 2) (make-point 4 6)))
(print-point (midpoint-segment line))

