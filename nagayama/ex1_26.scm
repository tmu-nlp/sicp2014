#lang scheme

; speci -- base^exp (mod m)
(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
         (else
         (remainder (* base (expmod base (- exp 1 ) m))
                    m)))) 


; Luis's --  base^exp (mod m)
(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                     m))
         (else
         (remainder (* base (expmod base (- exp 1 ) m))
                    m)))) 

; そもそも、O(log(n)) になったのは、
; 指数が偶数のときに、指数を 1/2 にしていく手法のおかげで
; ステップ数がおよそ 1/2 になったからである。
; 
; Luis's は(expmod)を2倍呼び出すことで、
; ステップ数が 1/2 になった恩得を打ち消している。
;

