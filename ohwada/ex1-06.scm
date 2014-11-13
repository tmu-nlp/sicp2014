; cond をつかって if (new-if) を定義

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(print (new-if (= 2 3) 0 5))
(print (new-if (= 1 1) 0 5))


; square, average, improve, good-enough? を定義

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


; sqrt-iter の if を new-if に変更 

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(print (sqrt-iter 1.0 8))

