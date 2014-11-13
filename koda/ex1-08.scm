(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess) ) 3 ))
(define (cube-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-iter (improve guess x) x)))

(print (cube-iter 1 8.0))
(print (cube-iter 1 27.0))
(print (cube-iter 1 64.0))
