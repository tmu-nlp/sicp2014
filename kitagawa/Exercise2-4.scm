#!/usr/bin/gosh
;;execution gosh Exercise2-4.scm

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define z (cons 3 4))
(print "(define z (cons 3 4))")
(print )
(print "(car z)")
(print (car z))
(print )
(print "(cdr z)")
(print (cdr z))
(print )

; (caa (conz 1 2))
; → (caa (lambda (m) (m 1 2)))
; → ((lambda (m) (m 1 2)) (lambda (p q) p))
; → ((lambda (p q) p) 1 2)
; → 1
