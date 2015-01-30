(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))


;;; 使用例
(define v1 (make-vect 1 2))
(define v2 (make-vect 5 8))

(xcor-vect (add-vect v1 v2))            ; 6
(ycor-vect (add-vect v1 v2))            ; 10

(xcor-vect (sub-vect v2 v1))            ; 4
(ycor-vect (sub-vect v2 v1))            ; 6

(xcor-vect (scale-vect 3 v1))           ; 3
(ycor-vect (scale-vect 6 v1))           ; 12
