; フレームの constructor と selector
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))


(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))



; ベクトルの constructor と selector
(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))


(define (add-vect a b)
  (make-vect
   (+ (xcor-vect a) (xcor-vect b)) (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b)) (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s a)
  (make-vect
   (* s (xcor-vect a)) (* s (ycor-vect a))))



; frame-coord-map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))



; segment の constructor と selector
(define (make-segment vect1 vect2) (cons vect1 vect2))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))



; draw-line (2点間に線を引く)
(define (draw-line v1 v2)
  (display (xcor-vect v1))
  (display ",")
  (display (ycor-vect v1))
  (display ",")
  (display (xcor-vect v2))
  (display ",")
  (display (ycor-vect v2))
  (newline))




; segments->painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))




; フレームX を定義
(define X (make-frame (make-vect 200 200) (make-vect 100 0) (make-vect 0 100)))



; 指定されたフレームの外形を描くペインタ
(define outline-frame
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0)))))


(print (outline-frame X))





; transform-painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))



; below (beside に似た方法)
(define (below painter1 painter2)
  (let ((paint-below
         (transform-painter painter1
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 0.0)
                            (make-vect 0.0 0.5)))
        (paint-above
         (transform-painter painter2
                            (make-vect 0.0 0.5)
                            (make-vect 1.0 0.5)
                            (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-below frame)
      (paint-above frame))))


(print ((below outline-frame outline-frame) X))



; beside と回転演算を用いた below
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


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))



(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))


(print ((below outline-frame outline-frame) X))
