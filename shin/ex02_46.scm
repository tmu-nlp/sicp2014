(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect vect1 vect2)
	(make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
		       (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
	(make-vect (- (xcor-vect vect1) (xcor-vect vect2))
		       (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scala vect1)
	(make-vect (* (xcor-vect vect1) scala)
		       (* (ycor-vect vect1) scala)))