; #lang scheme

; 連分数 から tan(x) の近似値を
; 求める手続き (tan-cf x k) を定義する.



; cont-frac
(define (cont-frac n d k)
  (define (iter i)
    (if (> k i)
        (/ (n i) (+ (d i) (iter (+ i 1))))
        (/ (n i) (d i))))
  (iter 1))

;; tan-cf
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))



; 以下、実行テスト


; 直接代入
; (tan-cf (/ pi 6) 10)  ; -> 0.5773502691896257
; (tan (/ pi 6))        ; -> 0.5773502691896256


; ex1.37と同様に, 4桁の精度で実験
(define (tan-in x)
  (define (iter x k)
    (if (close-enough? (tan-cf x k) (tan x))
        (begin (display "k   = ")
               (display k)
               (newline)
               (display "tcf = ")
               (display (tan-cf x k))
               (newline)
               (display "tan = ")
               (display (tan x)))
        (iter x (+ k 1))))
  (iter x 1))


; run 
(tan-in (/ pi 4))

; 結果
; k   = 3
; tcf = 0.9997876809149684
; tan = 0.9999999999999999
;