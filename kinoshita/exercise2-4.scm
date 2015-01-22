(define (cons x y)
    (lambda (m) (m x y)))
(define (car z)
    (z (lambda (p q) p)))
(define (cdr z)
    (z (lambda (p q) q)))

(define a (cons 4 9))
(print (car a))
(print (cdr a))
;(car a)
;(car (m 4 9))
;((m 4 9) (lambda (p q) p))
;((lambda (p q) p) 4 9)
;4