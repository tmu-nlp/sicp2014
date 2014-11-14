#usr/bin/gosh
 -*- coding:utf-8 -*-
;;実行方法　gosh ex01-17.scm

(define (double x) (* x 2)) 

(define (halve x) (/ x 2)) 

(define (even? n) (= (remainder n 2) 0)) 

(define (multiply x y)
  (cond ((= x 0) 0)
        ((= y 0) 0)
        ((= x 1) y)
        ((even? x) (multiply (halve x) (double y)))
        (else (+ y (multiply (- x 1) y)))))

(define (mul a b) (multiply a b))

(print "(mul 3 4)")
(print (mul 3 4))

(print "(mul 2 -4)")
(print (mul 2 -4))


