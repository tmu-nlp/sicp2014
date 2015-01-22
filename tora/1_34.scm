#lang racket
(define (f g)
  (g 2))
;(f f)
;;->
;(f (lambda (g)
;     (g 2)))
;;->
;((lambda (g)
;     (g 2))
;  (lambda (g)
;    (g 2)))
;;->
;((lambda (g) 
;  (g 2))
; 2)
;;->
;(2 2)
;;解釈できない
