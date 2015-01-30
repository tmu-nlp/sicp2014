#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define wave einstein)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (split func1 func2)
  (lambda (painter n)
    (let ((smaller (if (= n 0) painter ((split func1 func2) painter (- n 1)))))
      (func1 painter (func2 smaller smaller)))))

(define right-split (split beside below))
(define up-split (split below beside))

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

(paint (corner-split wave 4))