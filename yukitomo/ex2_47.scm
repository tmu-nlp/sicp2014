;ex2_47.scm
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

;define origin, edge1, edge2
(define origin-vec (make-vect 1 1))
(define edge-vec1 (make-vect 3 4))
(define edge-vec2 (make-vect 2 7))


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

;test1
(define frame (make-frame origin-vec edge-vec1 edge-vec2))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)


(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

;test2
(define frame (make-frame origin-vec edge-vec1 edge-vec2))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)
