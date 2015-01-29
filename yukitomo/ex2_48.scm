;ex2_48.scm

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (make-segment vect1 vect2) (cons vect1 vect2))
(define (start-segment segment)	(car segment)) ;原点から線分の始点へ向うベクタ
(define (end-segment segment) (cdr segment)) ;始点から線分の終点へ向うベクタ

;test
(define v1 (make-vect 3 4))
(define v2 (make-vect 2 7))
(define segment (make-segment v1 v2))
(start-segment segment)
(end-segment segment)
