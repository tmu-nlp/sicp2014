#!/usr/bin/gosh
(load "./ex01_35.scm")

(define (average x y) (/ (+ x y) 2))

;;----------------------------------------------------------
;(define tolerance 0.00001)

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (display guess)
;      (newline)
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))
;;----------------------------------------------------------

(print "平均緩和法を使わない場合")
#?=(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(print "平均緩和法を使った場合")
#?=(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
