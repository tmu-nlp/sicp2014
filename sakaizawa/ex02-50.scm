#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

;preparation
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

(define (make-frame origin edge1 edge2) ;各変数は対で表される
  (list origin edge1 edge2))

(define (origin-frame frame)
	(car frame))

(define (edge1-frame frame)
	(cadr frame))

(define (edge2-frame frame)
	(caddr frame))

(define (make-segment vect1 vect2) ;vectは対で表される
	(cons vect1 vect2))

(define (start-segment segment)
	(car segment))

(define (end-segment segment)
	(cdr segment))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; 新しいorigin
                     (make-vect 1.0 1.0)   ;edge1の新しい端点
                     (make-vect 0.0 0.0))) ;edge2の新しい端点

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))



;ans
(define (non-change painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))

(define (flip-horiz painter);non-changeからx軸方向の操作を逆にすれば良い
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

