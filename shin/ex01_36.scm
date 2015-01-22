#!/usr/bin/gosh
;;-----------------------------------------------
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)));next>guess
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;;-----------------------------------------------

(define (average x y) (/ (+ x y) 2))

;(print "平均緩和法を使わない場合33回")
;#?=(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;アベレージダンピング
;(print "平均緩和法を使った場合8回")
;#?=(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
