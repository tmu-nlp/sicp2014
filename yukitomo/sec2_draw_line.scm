;http://jots-jottings.blogspot.jp/2011/10/sicps-picture-language-in-drracket.html
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
(define (start-segment segment) (car segment)) ;原点から線分の始点へ向うベクタ
(define (end-segment segment) (cdr segment)) ;始点から線分の終点へ向うベクタ

(define (build-segments-list . vect-list)
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

;example
(send frame show #t)
(define spiral
  (segments->painter (build-segments-list (make-vect 0.0 0.0)
                                          (make-vect 1.0 0.0)
                                          (make-vect 1.0 1.0)
                                          (make-vect 0.0 1.0)
                                          (make-vect 0.0 0.1)
                                          (make-vect 0.9 0.1)
                                          (make-vect 0.9 0.9)
                                          (make-vect 0.1 0.9)
                                          (make-vect 0.1 0.2)
                                          (make-vect 0.8 0.2)
                                          (make-vect 0.8 0.8)
                                          (make-vect 0.2 0.8)
                                          (make-vect 0.2 0.3)
                                          (make-vect 0.7 0.3)
                                          (make-vect 0.7 0.7)
                                          (make-vect 0.3 0.7)
                                          (make-vect 0.3 0.4)
                                          (make-vect 0.6 0.4)
                                          (make-vect 0.6 0.6)
                                          (make-vect 0.4 0.6)
                                          (make-vect 0.4 0.5)
                                          (make-vect 0.5 0.5))))

(spiral window)
