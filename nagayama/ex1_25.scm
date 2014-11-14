#lang scheme

; 前提関数
(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

; fast-expt
(define (fast-expt b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* b a)))))
  (iter b n 1))

; correct?
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; 大きい数の剰余計算が含まれているため、
; 丸め込みや桁落ちが発生しうる。


; base^exp (mod m)
(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
         (else
         (remainder (* base (expmod base (- exp 1 ) m))
                    m)))) 

; 1回乗算をするたびに剰余計算を挟むため、
; 丸め込みや桁落ちが発生しにくい。
