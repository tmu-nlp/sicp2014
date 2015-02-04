#lang racket
(require racket/gui/base)

;http://jots-jottings.blogspot.jp/2011/10/sicps-picture-language-in-drracket.html


;vector
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect vect1 vect2)
	(make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
		       (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
	(make-vect (- (xcor-vect vect1) (xcor-vect vect2))
		       (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scala vect1)
	(make-vect (* (xcor-vect vect1) scala)
		       (* (ycor-vect vect1) scala)))

;segment
(define (make-segment vect1 vect2) (cons vect1 vect2))
(define (start-segment segment)	(car segment)) ;原点から線分の始点へ向うベクタ
(define (end-segment segment) (cdr segment)) ;始点から線分の終点へ向うベクタ

;与えられたベクトル(点)を満たすようなセグメントのリストを生成
(define (build-segments-list . vect-list)
  ;cur-vect:着目している点 remaining:残り点
  (define (build cur-vect remaining)
    (if (null? remaining)
        (list )
        (cons (make-segment cur-vect (car remaining))
              (build (car remaining) (cdr remaining)))))
  (if (null? vect-list)
      (list )
      (build (car vect-list) (cdr vect-list))))

;frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;draw-line
(define picture-size 300)

(define bitmap
  (make-object bitmap% (+ picture-size 1) (+ picture-size 1)))

(define bitmap-dc
  (new bitmap-dc% [bitmap bitmap]))

(define frame
  (new frame% [label "SICP Picture Language"]))

(define canvas
  (new canvas%
     [parent frame]
     [min-width (+ picture-size 1)]
     [min-height (+ picture-size 1)]
     [paint-callback (lambda (canvas dc)
                       (send dc draw-bitmap bitmap 0 0))]))

(define (draw-line start end)
  (send bitmap-dc 
        draw-line
        (xcor-vect start)
        (ycor-vect start)
        (xcor-vect end)
        (ycor-vect end)))

(define window (make-frame (make-vect 0 picture-size)
                           (make-vect picture-size 0)
                           (make-vect 0 (- 0 picture-size))))



;ex2_49.scm
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;window 
(send frame show #t)

;a. outline
(define outline
  (segments->painter (build-segments-list (make-vect 0.0 0.0)
                                          (make-vect 0.0 1.0)
                                          (make-vect 1.0 1.0)
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.0 0.0))))
;(outline window)

;b. diamond
(define diamond
  (segments->painter (build-segments-list (make-vect 0.0 0.5)
                                          (make-vect 0.5 1.0)
                                          (make-vect 1.0 0.5)
                                          (make-vect 0.5 0.0)
                                          (make-vect 0.0 0.5))))
;(diamond window)

;d. wave
(define wave
  (segments->painter
     (append (build-segments-list (make-vect 0.0  0.85)
                                (make-vect 0.15 0.6)
                                (make-vect 0.3  0.65)
                                (make-vect 0.4  0.65)
                                (make-vect 0.35 0.85)
                                (make-vect 0.4  1.0))
           (build-segments-list (make-vect 0.6  1.0)
                                (make-vect 0.65 0.85)
                                (make-vect 0.6  0.65)
                                (make-vect 0.75 0.65)
                                (make-vect 1.0  0.35))
           (build-segments-list (make-vect 1.0  0.15)
                                (make-vect 0.6  0.45)
                                (make-vect 0.75 0.0))
           (build-segments-list (make-vect 0.6  0.0)
                                (make-vect 0.5  0.3)
                                (make-vect 0.4  0.0))
           (build-segments-list (make-vect 0.25 0.0)
                                (make-vect 0.35 0.5)
                                (make-vect 0.3  0.6)
                                (make-vect 0.15 0.4)
                                (make-vect 0.0  0.65)))))
;(wave window)

;ex2_50.scm

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

;上下逆転
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; 新しいorigin
                     (make-vect 1.0 1.0)   ;edge1の新しい端点
                     (make-vect 0.0 0.0))) ;edge2の新しい端点

;与えられたフレームの右上の四半分に画像を縮めるペインタ
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

;画像を反時計回りに90度回転
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;画像をフレームの中央へ押し込む変換
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;((shrink-to-upper-right wave) window)

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


(define (non-change painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))

(define (flip-horiz painter)
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

;ex2_51.scm
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

((below wave diamond) window)