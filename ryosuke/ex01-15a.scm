; run follow
; gosh ex01-15.scm | grep CALL | wc -l
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine(/ angle 3.0)))))

(use slib)
(require `trace)
(trace p)
(sine 12.15)

