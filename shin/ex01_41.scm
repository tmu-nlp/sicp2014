#!/usr/bin/gosh

(define (inc x) (+ x 1))
(define (double f)
  (lambda (x) (f (f x))))

#?=(((double (double double)) inc) 5)

;y = (double double) 4回やる手続き
;lambda (x) (y (y x))
;は4回やる手続きを4回やることに！