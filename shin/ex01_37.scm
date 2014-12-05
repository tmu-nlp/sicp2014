#!/usr/bin/gosh
;;再帰的手続き
(define (cont-frac-rec n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

;;反復的手続き
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))
;;----------------------------------------------------------
(define cont-frac cont-frac-rec)
;(define cont-frac cont-frac-iter)

#?=(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 9)
#?=(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
#?=(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
#?=(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
#?=(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13)
#?=(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 14)
;4桁の精度を得るためには、k を11以上にすればよい