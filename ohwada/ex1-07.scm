
(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(print (sqrt-iter 1.0 0.000009))
(print (sqrt-iter 1.0 800000000000000000000000000000000000000000000))

; 上の sqrt-iter は非常に小さい数においては正確な結果が出ず、
; 非常に大きい数においては処理に時間がかかる。

; good-enough? と sqrt-iter を変更

(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.001))


(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess x))) 

(print (sqrt-iter 1.0 0.9 0.000009))
(print (sqrt-iter 1.0 0.9 800000000000000000000000000000000000000000000))
