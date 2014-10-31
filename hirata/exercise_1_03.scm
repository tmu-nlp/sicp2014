#!/usr/local/bin/gosh

(define (root a b c)
  (define (sauare x) (* x x))
  (if (> a b)
    (+ (square a) (square (if (> b c) b c)))
    (+ (square b) (square (if (> a c) a c))))
  )
(print (root 1 3 5))
