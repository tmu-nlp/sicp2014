; 立方根の近似値の計算

(define (cube x) (* x x x))

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* guess 2))
     3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))


(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
                      x)))

(print (cube-root-iter 1.0 27))
(print (cube-root-iter 1.0 12))
