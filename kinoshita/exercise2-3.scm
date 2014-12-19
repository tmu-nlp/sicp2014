(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(define (make-point x y)
    (cons x y))
(define (x-point p)
    (car p))
(define (y-point p)
    (cdr p))
(define (equal-point? p q)
    (if (and (= (x-point p) (x-point q))
             (= (y-point p) (y-point q)))
        #t
        #f))

(define (make-segment p q)
    (cons p q))
(define (start-segment s)
    (car s))
(define (end-segment s)
    (cdr s))

(define (midpoint-segment s)
    (make-point
        (/ (+ (x-point (start-segment s))
              (x-point (end-segment s)))
           2)
        (/ (+ (y-point (start-segment s))
              (y-point (end-segment s)))
           2)))

(define (make-rectangle p q)
    (cons p q))
(define (make-rectangle2 edge-ver edge-hor)
    (cond
        ((equal-point?
            (start-segment edge-ver)
            (start-segment edge-hor))
        (cons (end-segment edge-ver) (end-segment edge-hor)))
        ((equal-point?
            (end-segment edge-ver)
            (end-segment edge-hor))
        (cons (start-segment edge-ver) (start-segment edge-hor)))
        ((equal-point?
            (start-segment edge-ver)
            (end-segment edge-hor))
        (cons (end-segment edge-ver) (start-segment edge-hor)))
        ((equal-point?
            (end-segment edge-ver)
            (start-segment edge-hor))
        (cons (start-segment edge-ver) (end-segment edge-hor)))))
(define (apex-point1 rect)
    (car rect))
(define (apex-point2 rect)
    (cdr rect))

(define (area rect)
    (let ((width
            (abs
                (- (x-point (apex-point1 rect))
                   (x-point (apex-point2 rect)))))
          (height
            (abs
                (- (y-point (apex-point1 rect))
                   (y-point (apex-point2 rect))))))
         (* width height)))
(define (perimeter rect)
    (let ((width
            (abs
                (- (x-point (apex-point1 rect))
                   (x-point (apex-point2 rect)))))
          (height
            (abs
                (- (y-point (apex-point1 rect))
                   (y-point (apex-point2 rect))))))
         (* (+ width height) 2)))

(define a (make-point 1 1))
(define b (make-point 1 5))
(define c (make-point 5 5))
(define d (make-point 5 1))

(print (area (make-rectangle a c)))
(print (perimeter (make-rectangle a c)))

(define ver (make-segment a b))
(define hor (make-segment a d))
(define rect (make-rectangle2 ver hor))

(print (area rect))
(print (perimeter rect))
