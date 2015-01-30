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


;ans
;http://sandbox.serendip.ws/sicp_drawing.htmlで描画できるようにdraw-lineを定義しました
(define (draw-line vect1 vect2)
	(print (car vect1)","(cdr vect1)","(car vect2)","(cdr vect2)))

;a
(define draw-frame-outline
  (let ((v0 (make-vect 0.0 0.0))
        (v1 (make-vect 1.0 0.0))
        (v2 (make-vect 1.0 1.0))
        (v3 (make-vect 0.0 1.0)))
       (segments->painter
         (list (make-segment v0 v1)
               (make-segment v1 v2)
               (make-segment v0 v3)
               (make-segment v3 v2)))))

(draw-frame-outline (make-frame (make-vect 100 100) (make-vect 100 0) (make-vect 0 100)))

;b
(define draw-frame-X
  (let ((v0 (make-vect 0.5 0.5))
        (v1 (make-vect 1.0 0.0))
        (v2 (make-vect 1.0 1.0))
        (v3 (make-vect 0.0 1.0))
        (v4 (make-vect 0.0 0.0)))
       (segments->painter
         (list (make-segment v0 v1)
               (make-segment v0 v2)
               (make-segment v0 v3)
               (make-segment v0 v4)))))

(draw-frame-X (make-frame (make-vect 100 100) (make-vect 100 0) (make-vect 0 100)))

;c
(define draw-diamond
  (let ((v0 (make-vect 0.0 0.5))
        (v1 (make-vect 0.5 0.0))
        (v2 (make-vect 1.0 0.5))
        (v3 (make-vect 0.5 1.0)))
       (segments->painter
         (list (make-segment v0 v1)
               (make-segment v1 v2)
               (make-segment v2 v3)
               (make-segment v3 v0)))))

(draw-diamond (make-frame (make-vect 100 100) (make-vect 100 0) (make-vect 0 100)))

;d これは他の人のを見ました
(define wave
  (segments->painter
    (list (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
          (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
          (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65))
          (make-segment (make-vect 0.40 0.65) (make-vect 0.30 0.65))
          (make-segment (make-vect 0.75 0.65) (make-vect 1.00 0.35))
          (make-segment (make-vect 0.60 0.45) (make-vect 1.00 0.15))
          (make-segment (make-vect 0.60 0.45) (make-vect 0.75 0.00))
          (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
          (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.60))
          (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40))
          (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85))
          (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65))
          (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.50))
          (make-segment (make-vect 0.35 0.50) (make-vect 0.25 0.00))
          (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00)))))

(wave (make-frame (make-vect 100 100) (make-vect 100 0) (make-vect 0 100)))























