#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-36.scm

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (fixed-point f first-guess)
  (define (try guess)
;   (let ((next (average guess (f guess)))) ; 平均緩和法を使用する場合
    (let ((next (f guess)))
      (display guess) (newline) ; ← ここに追加
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;;平均緩和法を使用しない（注釈はずさない）
(f (lambda (x) (/ (log 1000) (log x)))

;;平均緩和法を使用（注釈をはずす）
;;(f (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)



