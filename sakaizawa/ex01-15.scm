#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;;pが呼び出された時のアングル、括弧内は呼び出された回数
;;angle 12.15(1), 4.05(2), 1.35(3), 0.45(4), 0.15(5)

;;angleの値がaの値が3倍になると1増えるのでΘ(log3 a)となる


