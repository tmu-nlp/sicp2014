#!/usr/bin/gosh

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x)))) 
(define (sine angle)
  (if (not (> (abs angle) 0.1)) 
      angle
      (p (sine (/ angle 3.0))) ))

(print (sine 12.15))

;-------

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle c)
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0) #?=(+ c 1)))))

#?=(sine 12.15 0)

;;pは5回呼び出される。
;;スペース:再帰なのでΘ(log(a))
;;ステップ数:aが3で割られ続けるのでΘ(log(a))