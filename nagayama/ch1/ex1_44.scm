; #lang scheme

; 関数の平滑化手続き smooth の定義.
; n 回 smooth を繰り返す n-fold-smooth の定義.

; 前提関数
(define (square x) (* x x))
(define dx 0.01)


; 繰り返し : repeated
(define (repeated f n)
  (lambda (x)
    (let loop ((i 1)
               (x x))
      (if (> i n)
          x
          (loop (+ i 1) (f x))))))

; 補完 : smooth
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

; n次畳み込み補完 : n-fold-smooth
(define (n-fold-smooth f n)
  ((repeated smooth n) f))




