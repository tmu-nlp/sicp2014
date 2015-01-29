; #lang scheme

; 関数の合成をする手続き compose の定義.
; (compose f g) -> (f (g x))


; 前提関数
(define (inc x) (+ x 1))
(define (square x) (* x x))


; compose
(define (compose f g)
  (lambda (x) (f (g x))))


; run
((compose square inc) 6)
; -> 49

