#lang scheme

; 和, 2倍, 半分, を用いて乗算手続き

; double, halve
(define (double x) (* x 2))
(define (halve x) (/ x 2))

; 偶奇判定
(define (even? n) (= (remainder n 2) 0))

; fast-mult
(define (fast-mult b n)
  (cond ((= n 1) b)
        ((even? n) (double (fast-mult b (halve n))))
        (else (+ b (fast-mult b (- n 1))))))

;  b * n
;  b * (n-1)  + b
; (b *  n/2 ) * 2

; run
(fast-mult 11 9)
; -> 99
