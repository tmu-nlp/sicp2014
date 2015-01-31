; $B%Y%/%H%k$N(B constructor $B$H(B selector
(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))


; $B%Y%/%H%k$N2C;;!&8:;;!&%9%+%i!<$N>h;;(B
(define (add-vect a b)
  (make-vect
   (+ (xcor-vect a) (xcor-vect b)) (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b)) (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s a)
  (make-vect
   (* s (xcor-vect a)) (* s (ycor-vect a))))


(define X (make-vect 10 6))
(define Y (make-vect 4 17))


(print (add-vect X Y)) ; (14 . 23)
(print (sub-vect X Y)) ; (6 . -11)
(print (scale-vect 3 X)) ; (30 . 18)
