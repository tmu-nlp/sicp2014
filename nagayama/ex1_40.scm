; #lang scheme

; cubic を定義し, newton-method 手続きを利用して
; 三次方程式の近似解を求める.

; 前提関数
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (close-enough? x y) (< (abs (- x y)) 0.001))


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

; 手続きに対して微分
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

; 不動点処理によるニュートン法
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


; cubic
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))


; run
(newtons-method (cubic -2 4 -8) 1)

; x^3 - 2x^2 + 4x - 8 = 0  ; -> 2.0000000000000147



