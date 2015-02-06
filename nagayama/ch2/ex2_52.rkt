#lang racket

; square-limit
; [a] wave に線を追加（例えば笑顔）
; [b] corner-split 
; [c] square-of-four の書き換え


(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; [a]
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

(define w010 (make-vect 0.45 0.80))
(define w020 (make-vect 0.52 0.75))
(define w030 (make-vect 0.58 0.80))

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
         
         (make-segment w010 w020)
         (make-segment w020 w030)         
         )))


; [b] corner-split を書き換えて表示パターンを変更する
;     簡単に右に表示される画像を横向きにした

(define (corner-split painter n)
  (if (= n 0)
      (rotate180 painter)
      (let ((up    (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; [c] square-of-four を変更して角に表示されるパターンを変更する

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four rotate90 rotate270
                                  rotate270 rotate90)))
    (combine4 (corner-split painter n))))


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

; 出力
(paint wave)
(paint (corner-split einstein 2))
(paint (square-limit einstein 2))


