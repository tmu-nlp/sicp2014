;http://jots-jottings.blogspot.jp/2011/10/sicps-picture-language-in-drracket.html
;http://jots-jottings.blogspot.jp/search?q=+build-segments-list
#lang racket
(require racket/gui/base)

;vector
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
   
(define wave2
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
(wave2 window)