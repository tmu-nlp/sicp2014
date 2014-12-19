; #lang scheme

; 前提関数
(define (average x y) (/ (+ x y) 2))
(require srfi/27)

 
; 反復改善法
(define (iterative-improve enough? improve)
  (lambda (guess)
    (let loop ((guess guess))
      (let ((next (improve guess)))
        (display guess)
        (display " --- ")
        (display next)
        (newline)
        (if (enough? guess next)
          next
          (loop next))))))

; 推測値評価
(define tolerance 0.00001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2))
     tolerance))


; 不動点調査 : fixed-point
(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

; sqrt
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) x))


; run
(sqrt 2.0)
          