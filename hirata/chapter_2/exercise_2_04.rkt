(define (cons x y) 
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(define test (cons 1 3))
;(display (car test))  ;ちゃんとcarが"1"を返すことを確認．

(define (cdr z)
  (z (lambda (p q) q)))

(display (cdr test))