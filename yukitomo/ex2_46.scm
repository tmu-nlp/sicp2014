;ex2_46.scm

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))


;ベクトル足し算
(define (add-vect vect1 vect2)
	(make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
		       (+ (ycor-vect vect1) (ycor-vect vect2))))
;ベクトル引き算
(define (sub-vect vect1 vect2)
	(make-vect (- (xcor-vect vect1) (xcor-vect vect2))
		       (- (ycor-vect vect1) (ycor-vect vect2))))
;ベクトルの定数倍
(define (scale-vect scala vect1)
	(make-vect (* (xcor-vect vect1) scala)
		       (* (ycor-vect vect1) scala)))

(define v1 (make-vect 3 4))
(define v2 (make-vect 2 7))

(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect 3 v1)