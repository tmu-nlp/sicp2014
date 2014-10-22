(define (sqrt-iter guess x)
;(if (good-enough? guess x)
(if (good-enough? guess (improve guess x))
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x) 0.001))
(define (good-enough? guess improved)
  (< (abs (- guess improved)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))

(print (sqrt 0.00001))
(print (sqrt 10000000000000))

