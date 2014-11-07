

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle c)
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0) (+ c 1)))))

(sine 12.15 0)