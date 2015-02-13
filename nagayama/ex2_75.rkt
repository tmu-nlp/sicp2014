#lang racket

; メッセージパッシング形式で make-from-mag-ang を定義する.
; 

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)


; 実行テスト
(define test-complex (make-from-mag-ang 2.0 (/ pi 3)))

(test-complex 'real-part)
(test-complex 'imag-part)
(test-complex 'magnitude)
(test-complex 'angle)
; (test-complex 'error)

