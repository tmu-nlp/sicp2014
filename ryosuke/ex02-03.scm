(define (make-point x y)
  (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))

;
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (height-rectangle x)
  (- (cdr (cdr x)) (cdr (car x))))

(define (width-rectangle x)
  (- (car (cdr x)) (car (car x))))

(define (area-rectangle x)
  (* (height-rectangle x) (width-rectangle x)))

(define (perimeter-rectangle x)
  (+ (* 2 (height-rectangle x)) (* 2 (width-rectangle x))))


(define rect (make-rectangle (make-point 2 2) (make-point 4 6)))
(print "area")
(print (area-rectangle rect))
(print "perimeter")
(print (perimeter-rectangle rect))




