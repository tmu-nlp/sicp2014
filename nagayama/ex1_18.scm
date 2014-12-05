; #lang scheme

; ex1.17 で設計した乗算手続きを反復プロセスで書き直す.

; double, halve
(define (double x) (* x 2))
(define (halve x) (/ x 2))

; 偶奇判定
(define (even? n) (= (remainder n 2) 0))

; fast-mult
(define (fast-mult b n)
  (define (iter b n a)
    (cond ((= n 1) (+ b a))
          ((even? n) (iter (double b) (halve n) a))
          (else (iter b (- n 1) (+ b a)))))
  (iter b n 0))

;    b     *  n
; = (b*2)  * (n/2)
; =  NEW b *  NEW n
; =  ...
; =  ans   *  1 

; run
(fast-mult 11 9)
; -> 99
