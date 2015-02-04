#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define one 0.99)
(define origin (make-vect 0 0))
(define lower-right (make-vect one 0))
(define upper-left (make-vect 0 one))
(define upper-right (make-vect one one))

(define (outline frame)
    ((segments->painter
        (list
         (make-segment origin lower-right)
         (make-segment lower-right upper-right)
         (make-segment upper-right upper-left)
         (make-segment upper-left origin)))
     frame))

(paint outline)

(define (diagonal frame)
    ((segments->painter
        (list
            (make-segment origin upper-right)
            (make-segment lower-right upper-left)))
     frame))

(paint diagonal)

(define mid-left (make-vect 0 0.5))
(define mid-right (make-vect one 0.5))
(define mid-top (make-vect 0.5 one))
(define mid-bottom (make-vect 0.5 0))

(define (diamond frame)
    ((segments->painter
         (list
             (make-segment mid-left mid-top)
             (make-segment mid-top mid-right)
             (make-segment mid-right mid-bottom)
             (make-segment mid-bottom mid-left)))
     frame))

(paint diamond)

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
         )))

(paint wave)