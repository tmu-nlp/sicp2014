; #lang scheme

; 手続きを 2回適用する手続き double を定義する.
; (double f) -> (f (f x))
 

; 前提関数
(define (inc x) (+ x 1))

; double
(define (double f)
  (lambda (x) (f (f x))))


; run
(((double (double double)) inc) 5)

; -> 21



