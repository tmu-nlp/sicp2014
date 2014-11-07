(define (cube-iter guess x)
(if (good-enough? guess (improve guess x))
    guess
    (cube-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(define (good-enough? guess improved)
  (< (abs (- guess improved)) 0.001))

(define (cube-root x)
  (cube-iter 1.0 x))

(define (square x)
  (* x x))

(print (cube-root 8))

