#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define w1 (make-vect 0.00 0.85))
(define w2 (make-vect 0.15 0.62))
(define w3 (make-vect 0.30 0.70))
(define w4 (make-vect 0.42 0.70))
(define w5 (make-vect 0.38 0.88))
(define w6 (make-vect 0.40 1.00))
(define w7 (make-vect 0.62 1.00))
(define w8 (make-vect 0.65 0.88))
(define w9 (make-vect 0.60 0.70))
(define w10 (make-vect 0.75 0.70))
(define w11 (make-vect 1.00 0.38))
(define w12 (make-vect 1.00 0.15))
(define w13 (make-vect 0.64 0.48))
(define w14 (make-vect 0.78 0.00))
(define w15 (make-vect 0.62 0.00))
(define w16 (make-vect 0.52 0.30))
(define w17 (make-vect 0.40 0.00))
(define w18 (make-vect 0.25 0.00))
(define w19 (make-vect 0.36 0.52))
(define w20 (make-vect 0.30 0.64))
(define w21 (make-vect 0.15 0.43))
(define w22 (make-vect 0.00 0.67))
(define w23 (make-vect 0.50 0.70))
(define w24 (make-vect 0.45 0.75))
(define w25 (make-vect 0.55 0.75))
(define w26 (make-vect 0.42 0.85))
(define w27 (make-vect 0.50 0.85))
(define w28 (make-vect 0.53 0.85))
(define w29 (make-vect 0.60 0.85))
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
         (make-segment w15 w16)
         (make-segment w16 w17)
         (make-segment w18 w19)
         (make-segment w19 w20)
         (make-segment w20 w21)
         (make-segment w21 w22)
         (make-segment w23 w24)
         (make-segment w23 w25)
         (make-segment w26 w27)
         (make-segment w28 w29)
         )))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 2)))
              (right (right-split painter (- n 2))))
            (let ((top-left (beside up up))
                  (bottom-right (below right right))
                  (corner (corner-split painter (- n 2))))
                (beside (below painter top-left)
                        (below bottom-right corner))))))

(define (split p1 p2)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split p1 p2) painter (- n 1))))
                (p1 painter (p2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let ((top (beside (tl painter) (tr painter)))
              (bottom (beside (bl painter) (br painter))))
            (below bottom top))))

(define (flipped-pairs painter)
    (let ((combine4 (square-of-four identity flip-vert
                                    identity flip-vert)))
        (combine4 painter)))

(define (square-limit painter n)
    (let ((combine4 (square-of-four rotate90 identity
                                    rotate180 rotate270)))
        (combine4 (corner-split painter n))))

(paint wave)
(paint (corner-split wave 4))
(paint (square-limit wave 4))