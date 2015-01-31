#lang racket

; up-split の定義
; 

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; up-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))



; 以下、実行テスト

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; wave  
(define w1 (make-vect 0.00 0.85))
(define w2 (make-vect 0.17 0.62))
(define w3 (make-vect 0.30 0.70))
(define w4 (make-vect 0.42 0.70))
(define w5 (make-vect 0.38 0.88))
(define w6 (make-vect 0.40 1.00))

(define w7 (make-vect 0.62 1.00))
(define w8 (make-vect 0.65 0.88))
(define w9 (make-vect 0.60 0.70))
(define w10 (make-vect 0.75 0.70))
(define w11 (make-vect 1.00 0.40))

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
(define w22 (make-vect 0.15 0.43))
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

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (flipped-pairs wave))
(define wave-right (right-split wave 2))
(define wave-corner (corner-split wave 2))

; 出力
(display "wave") (newline)
(paint wave) (newline)
(display "wave2") (newline)
(paint wave2) (newline)
(display "wave4") (newline)
(paint wave4) (newline)
(display "wave-right 2") (newline)
(paint wave-right) (newline)
(display "wave-corner 2") (newline)
(paint wave-corner) (newline)