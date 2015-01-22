;excersise 1.34

(define (square n) (* n n))

(define (f g) (g 2))

;(f square)
;4
;(f (lambda (z) (* z (+ z 1))))
;6

(f f)