; square-root

; 被開法数xがとても小さな数の場合
; 推定値の二乗とxとの差がもともと小さいため、
; 推定値が√xに近づく前に処理が終了してしまう。

; 被開法数xがとても大きな数の場合
; 1回の手続で√xに近づく量が微小になり、
; 推定値が√xに近づくまでのループ回数が膨大になってしまう。

; x の大小に応じて終了判定の評価値を調整する必要がある。

; メイン
(define (sqrt x)
  (define (iter guess x)
     (if (good-enough? guess x)
         guess
         (iter (inprove guess x) x)))
  (iter 1.0 x))

; 推定値の算出
(define (improve guess x)
  (newton guess (/ x guess)))

; ニュートン法(平均)
(define (newton x y)
  (/ (+ x y) 2))

; 終了判定
; ex1) x が 10^10  ならば評価値は10^2
; ex2) x が 10^-10 ならば評価値は10^-8
(define (good-enough? guess x)
  (< (abs (- (expo guess 2) x))
     (if (or (> x 10000) (< x 0.0001))
         (/ x (expo 10 (- (/ (figure x) 2) 3)))
         0.001)))

; log10
(define (figure x)
  (define (iter x fig)
          (if (and (< x 10)
                   (or (> x 0) (= x 0)))
              fig
              (if (< x 0)
                  (iter (* x 10) (- fig 1)) 
                  (iter (/ x 10) (+ fig 1)))))
  (iter x 1))

; 冪乗
(define (expo x y)
  (define (iter x y ans)
          (if (or (< y 0) (= y 0))
              ans
              (iter x (- y 1) (* ans x))))
  ((if (< y 0) / *)
   1
   (iter x (abs y) 1)))

; 絶対値
(define (abs x)
  (if (< x 0)
      (- x)
      x))

