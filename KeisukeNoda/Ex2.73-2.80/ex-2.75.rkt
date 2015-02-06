#lang racket

;; make-from-mag-angをメッセージパッシング調に直す問題.
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op: -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define z (make-from-mag-ang 1.0 (/ pi 6))) 
(z 'real-part) ; 0.8660254037844387
(z 'imag-part) ; 0.49999999999999994

;; apply-genericを通す.
(define (apply-generic op arg) (arg op))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(real-part z) ; 0.8660254037844387
(imag-part z) ; 0.49999999999999994