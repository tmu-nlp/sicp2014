#lang racket

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

; --- セレクタ ---

(define (origin-frame1 frame)
  (car frame))

(define (edge1-frame1 frame)
  (cadr frame))

(define (edge2-frame1 frame)
  (caddr frame))

(origin-frame1 (make-frame1 1 2 3))
(edge1-frame1 (make-frame1 1 2 3))
(edge2-frame1 (make-frame1 1 2 3))


; =====================================
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))

; --- セレクタ ---

(origin-frame2 (make-frame2 1 2 3))
(edge1-frame2 (make-frame2 1 2 3))
(edge2-frame2 (make-frame2 1 2 3))