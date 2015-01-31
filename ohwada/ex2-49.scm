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


; a. 指定されたフレームの外形を描くペインタ
(define outline-frame
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0)))))


(print (outline-frame X))



; b. フレームの向かい合う頂点同士を結んで"X"を描くペインタ
(define draw-x
  (segments->painter
    (list
     (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
     (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))


(print (draw-x X))


; c. フレームの各辺の中点を結んで菱形を描くペインタ
(define draw-rhombus
  (segments->painter
    (let ((mid1 (scale-vect 0.5
                  (add-vect (make-vect 0.0 0.0) (make-vect 1.0 0.0))))
          (mid2 (scale-vect 0.5
                  (add-vect (make-vect 0.0 0.0) (make-vect 0.0 1.0))))
          (mid3 (scale-vect 0.5
                  (add-vect (make-vect 1.0 0.0) (make-vect 1.0 1.0))))
          (mid4 (scale-vect 0.5
                  (add-vect (make-vect 0.0 1.0) (make-vect 1.0 1.0)))))
      (list
       (make-segment mid1 mid2)
       (make-segment mid1 mid3)
       (make-segment mid2 mid4)
       (make-segment mid3 mid4)))))


(print (draw-rhombus X))


; d. wave ペインタ
(define wave
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))

(print (wave X))
