; #lang scheme

; x -> 1+1/x の不動点は 黄金比φ であることを
; fixed-point 手続きを用いて示す.

; 前提関数
(define (average x y) (/ (+ x y) 2))

; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (phi x)
(define (phi x)
  (fixed-point (lambda (y) (average y (+ 1 (/ 1 y)))) 1.0))

; run
(phi 1) ; -> 1.6180311591702674