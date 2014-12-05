#lang scheme

;cube-rute
;ex1_7.scm をもとに、ニュートン法の式と要所の数値を修正
 
; メイン
(define (curt x)
  (define (iter guess x)
     (if (good-enough? guess x)
         guess
         (iter (improve guess x) x)))
  (iter 1.0 x))

; 推定値の算出
(define (improve guess x)
  (newton guess (/ x guess)))

; ニュートン法(三次)
(define (newton x y)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

; 終了判定
; ex.1) x が 10^15  ならば評価値は10^2
; ex.2) x が 10^-15 ならば評価値は10^-8
(define (good-enough? guess x)
  (< (abs (- (expo guess 3) x))
     (if (or (> x 1000000) (< x 0.000001))
         (/ x (expo 10 (- (/ (figure x) 3) 3)))
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

