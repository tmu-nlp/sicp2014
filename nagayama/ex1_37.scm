; #lang scheme

; k-項有限連分数 を計算する手続き cont-frac を定義する.
; 線形プロセスと再帰プロセスの両方を作る.
; 連分数 から 1/φ の近似値の計算.
; 少数点以下 4桁 の精度の近似を得る k の大きさを調べる.


; 前提関数
(define (average x y) (/ (+ x y) 2))

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

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; 前提関数 ここまで


; cont-frac (iterative)
(define (cont-frac n d k)
  (define (iter i result)
    (if (< 0 i)
        (iter (- i 1) (/ (n i) (+ (d i) result)))
        result))
  (iter k 0))

; cont-frac (recursive)
(define (cont-frac-li n d k)
  (define (iter i)
    (if (> k i)
        (/ (n i) (+ (d i) (iter (+ i 1))))
        (/ (n i) (d i))))
  (iter 1))


; 1/φ の近似値の計算
(define (phi-in)
  (define (iter k)
    (if (close-enough?
          (cont-frac (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     k)
          (/ 2 (+ 1 (sqrt 5))))
        (begin (display "k   = ")
               (display k)
               (newline)
               (display "c_f = ")
               (display (cont-frac (lambda (i) 1.0)
                                   (lambda (i) 1.0)
                                   k))
               (newline)
               (display "1/φ = ")
               (display (/ 2 (+ 1 (sqrt 5)))))
        (iter (+ k 1))))
  (iter 1))


; run
(phi-in)


; 結果
; k   = 8
; c_f = 0.6176470588235294
; 1/φ = 0.6180339887498588
;