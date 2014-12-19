(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(print (car (cons 1 2)))

(define (cdr z)
  (z (lambda (p q) q)))

(print (cdr (cons 1 2)))

; substitution model
; (car (cons 1 2))
; (car (lambda (m) (m 1 2)))
; ((lambda (m) (m 1 2)) (lambda (p q) q))
; ((lambda (p q) p) 1 2)
; 1

