(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


(define (make-segment vect1 vect2)
	(cons vect1 vect2))

(define (start-segment segment)
	(car segment))

(define (end-segment segment)
	(cdr segment))

(print (make-segment (make-vect 0 0) (make-vect 100 100)))

(print (start-segment (make-segment (make-vect 0 0) (make-vect 100 100))))

(print (end-segment (make-segment (make-vect 0 0) (make-vect 100 100))))

