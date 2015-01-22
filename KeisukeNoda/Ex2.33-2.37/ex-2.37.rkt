#lang racket

(require "public.rkt")

(define v1 '(1 3 -5))
(define v2 '(4 -2 -1))

(define v3 '(-2 1 0 5))
(define m1 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define m2 '((1 2 3 4) (5 6 7 8) (9 10 11 12)))

(define m3 '((1 2 -1) (3 4 0) (-1 2 -2)))
(define m4 '((3 -2) (1 0) (4 -3)))
  
(define (dot-product v w) 
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) 
           (map (lambda (col) 
                  (dot-product row col)) 
                cols)) 
         m)))


(dot-product v1 v2)
3

(matrix-*-vector m1 v3)
'(20 27 40)

(transpose m2)
'((1 5 9) (2 6 10) (3 7 11) (4 8 12))
  
(matrix-*-matrix m3 m4)
'((1 1) (13 -6) (-9 8))
  
  