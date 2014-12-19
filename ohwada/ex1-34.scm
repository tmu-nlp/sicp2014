(define (square x) (* x x))

(define (f g)
  (g 2))

(print (f square)) ; 4
(print (f (lambda (z) (* z (+ z 1))))) ; 6

(print (f f)) ; (f f) → (f 2) → (2 2) となりエラーになる
