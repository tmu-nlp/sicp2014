#!/usr/bin/gosh

;;---------------------------------------------
(define (square x) (* x x))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))
;;---------------------------------------------

#?=((repeated square 2) 5)