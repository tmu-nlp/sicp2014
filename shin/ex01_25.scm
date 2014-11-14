#!/usr/bin/gosh
(use srfi-19)
(use srfi-27)

(load "./ex01_24.scm")

;元々の expmod
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))

;Alyssa の expmod 
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(print "ex_25-------------------")
(search (expt 10 3))
(search (expt 10 4))
(search (expt 10 5))
(search (expt 10 6))
(search (expt 10 15))

;これは上と同じようには使えない. 理由はremainder に使う引数が巨大数になるため演算に時間がかかる。
;もともとの方法では各計算で剰余を求めることで値の増加を抑えつつ計算しているので、最終的な計算時間が短い。
