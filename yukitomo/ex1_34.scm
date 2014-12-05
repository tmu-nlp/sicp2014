;ex1_34
(define (square x) (* x x))
(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(f f)
;(f 2)
;(2 2)