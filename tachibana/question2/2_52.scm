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


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

((below wave wave) (list (cons 100 100) (cons 100 0) (cons 0 100)))


(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;ans
(define wave
  (segments->painter
    (list (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
          (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
          (make-segment (make-vect 0.50 0.75) (make-vect 0.42 0.78)) ;; 口
          (make-segment (make-vect 0.50 0.75) (make-vect 0.58 0.78)) ;; 口
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

(wave (list (cons 100 100) (cons 100 0) (cons 0 100)))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner)))))

(paint (corner-split wave 1))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))









































