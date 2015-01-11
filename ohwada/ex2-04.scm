(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))


; (car (cons x y)) $B$NI>2A$NN.$l(B

; (car (cons x y))
; ((cons x y) (lambda (p q) p))
; ((lambda (m) (m x y)) (lambda (p q) p))   $B"+(B (lamdba (p q) p) $B$r(B m $B$KF~$l$k(B
; ((lambda (p q) p) x y)   $B"+(B x $B$H(B y $B$r(B (p q) $B$KF~$l$k(B
; x


; cdr $B$NDj5A(B

(define (cdr z)
  (z (lambda (p q) q)))


; $B%F%9%H(B

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(print (car (car z))) ; 1
(print (cdr (car z))) ; 2
(print (car (cdr z))) ; 3
(print (cdr (cdr z))) ; 4
