#lang racket

; 画像の変形
; 左右反転, 180度回転, 270度回転

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define (flip-horizontal painter)
  (transform-painter2 painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotation180 painter)
  (transform-painter2 painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotation270 painter)
  (transform-painter2 painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; 以下, 実行テスト

; transform-painter
(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))


(define (flip-vertical painter)
  (transform-painter2 painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter2
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(define (rotation90 painter)
  (transform-painter2 painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; ベクトル操作
(define (xcor-vect vec) (car vec))
(define (ycor-vect vec) (cdr vec))
(define (add-vect s t)
  (make-vect (+ (xcor-vect s) (xcor-vect t))
             (+ (ycor-vect s) (ycor-vect t))))
(define (sub-vect s t)
  (make-vect (- (xcor-vect s) (xcor-vect t))
             (- (ycor-vect s) (ycor-vect t))))
(define (scale-vect s vec)
  (make-vect (* s (xcor-vect vec))
             (* s (ycor-vect vec))))

; wave
(define w1 (make-vect 0.00 0.75))
(define w2 (make-vect 0.19 0.57))
(define w3 (make-vect 0.33 0.70))
(define w4 (make-vect 0.45 0.68))
(define w5 (make-vect 0.35 0.82))
(define w6 (make-vect 0.46 1.00))

(define w7 (make-vect 0.56 1.00))
(define w8 (make-vect 0.67 0.82))
(define w9 (make-vect 0.57 0.68))
(define w10 (make-vect 0.72 0.70))
(define w11 (make-vect 1.00 0.30))

(define w12 (make-vect 1.00 0.20))
(define w13 (make-vect 0.70 0.55))
(define w14 (make-vect 0.64 0.48))
(define w15 (make-vect 0.78 0.00))

(define w16 (make-vect 0.62 0.00))
(define w17 (make-vect 0.52 0.30))
(define w18 (make-vect 0.40 0.00))

(define w19 (make-vect 0.25 0.00))
(define w20 (make-vect 0.40 0.50))
(define w21 (make-vect 0.35 0.56))
(define w22 (make-vect 0.17 0.45))
(define w23 (make-vect 0.00 0.65))

(define wave 
  (segments->painter 
   (list (make-segment w1 w2)
         (make-segment w2 w3)
         (make-segment w3 w4)
         (make-segment w4 w5)
         (make-segment w5 w6)
         
         (make-segment w7 w8)
         (make-segment w8 w9)
         (make-segment w9 w10)
         (make-segment w10 w11)
         
         (make-segment w12 w13)
         (make-segment w13 w14)
         (make-segment w14 w15)
         
         (make-segment w16 w17)
         (make-segment w17 w18)

         (make-segment w19 w20)
         (make-segment w20 w21)
         (make-segment w21 w22)
         (make-segment w22 w23)
         )))

; 出力
(paint wave)
(paint (flip-horizontal wave))
(paint (rotation180 wave))
(paint (rotation270 wave))
