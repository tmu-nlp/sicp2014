(define (make-vect x y)
    (cons x y))

(define (xcor-vect v)
    (car v))

(define (ycor-vect v)
    (cdr v))

(define (add-vect v w)
    (cons (+ (xcor-vect v) (xcor-vect w))
          (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
    (cons (- (xcor-vect v) (xcor-vect w))
          (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect v s)
    (cons (* s (xcor-vect v)) (* s (ycor-vect v))))

(define v (make-vect 2 3))
(define w (make-vect 4 5))

(print (add-vect v w))
(print (sub-vect v w))
(print (scale-vect v 3))


