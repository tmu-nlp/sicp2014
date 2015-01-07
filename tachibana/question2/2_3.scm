(define (make-rectangle height width) (cons height width))
(define (height-rectangle a) (car a))
(define (width-rectangle a) (cdr a))

(define (perimeter r)
  (+ (* 2 (height-rectangle r))
     (* 2 (width-rectangle r))))

(define (area r)
  (* (height-rectangle r) (width-rectangle r)))

(print "(perimeter (make-rectangle 10 50))")
(print (perimeter (make-rectangle 10 50)))

(print "(area (make-rectangle 10 50))")
(print (area (make-rectangle 10 50)))
