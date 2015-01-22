(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (avg a b) (/ (+ a b) 2))
(define (midpoint-segment seg)
  (cons
   (avg (x-point (start-segment seg))
        (x-point (end-segment seg)))
   (avg (y-point (start-segment seg))
        (y-point (end-segment seg)))))

(print "(midpoint-segment(make-segment (make-point 1 2) (make-point 3 4)))")
(print (midpoint-segment(make-segment (make-point 1 2) (make-point 3 4))))