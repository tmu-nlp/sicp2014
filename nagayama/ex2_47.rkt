#lang racket

; frame を list, cons のそれぞれで定義したときのセレクタの違い

; list で定義したとき
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

; list に対応したセレクタ
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

; cons で定義したとき
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; cons に対応したセレクタ
(define (origin-frame2 frame) (car frame))
(define (edge1-frame2 frame) (cadr frame))
(define (edge2-frame2 frame) (cddr frame))


; run
(define test-frame
  (make-frame (cons 0 0) (cons 1 0) (cons 0 1)))

test-frame
(origin-frame test-frame)
(edge1-frame test-frame)
(edge2-frame test-frame)
