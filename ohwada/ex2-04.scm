(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))


; (car (cons x y)) の評価の流れ

; (car (cons x y))
; ((cons x y) (lambda (p q) p))
; ((lambda (m) (m x y)) (lambda (p q) p))   ← (lamdba (p q) p) を m に入れる
; ((lambda (p q) p) x y)   ← x と y を (p q) に入れる
; x


; cdr の定義

(define (cdr z)
  (z (lambda (p q) q)))


; テスト

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(print (car (car z))) ; 1
(print (cdr (car z))) ; 2
(print (car (cdr z))) ; 3
(print (cdr (cdr z))) ; 4
