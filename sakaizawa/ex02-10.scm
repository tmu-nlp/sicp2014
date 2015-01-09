#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-10.scm

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;;修正前
(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;;問題点
;;新しく(make-interval)を呼び出しているが０をまたぐ場合
;;(-2 4)などによって新しく作られるものは(1/4 -1/2)
;;など大小が逆転しまう問題がある

;;修正
(define (new-div-interval x y)
  (let ((yl (lower-bound y))
        (yu (upper-bound y)))
    (if (and (<= yl 0) (<= 0 yu))
        (error "error" yl yu)
        (mul-interval x
                          (make-interval (/ 1.0 yu) (/ 1.0 yl))))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define r1 (make-interval 6 8))
(print "(define r1 (make-interval 6 8))")
(define r2 (make-interval 7 9))
(print "(define r2 (make-interval 7 9))")

(print "(new-div-interval r1 r2)")
(print (new-div-interval r1 r2))
(print )

(define r3 (make-interval -2 8))
(print "(define r3 (make-interval -2 8))")
(define r4 (make-interval -4 10))
(print "(define r4 (make-interval -4 10))")

(print "(new-div-interval r3 r4)")
(print (new-div-interval r3 r4))
(print )


