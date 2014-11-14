#!/usr/bin/gosh

(define (double x) (+ x x))
(define (halve x) (/ x 2))

;b = 0 の時,  a*0 = 0
;b が偶数の時,  2a * b/2
;b が奇数の時,  a + a * (b - 1)
(define (multi a b)
  (cond ((= b 0) 0)
        ((even? b) (multi (double a) (halve b)))
        (else (+ a (multi a (- b 1))))))

#?=(multi 7 8)

;(multi 7 8)
;(multi (double 7) (halve 8))
;(multi (* 7 2) (/ 8 2))
;(multi 14 4)
;(multi (double 14) (halve 4))
;(multi (* 14 2) (/ 4 2))
;(multi 28 2)
;(multi (double 28) (halve 2))
;(multi (* 28 2) (/ 2 2))
;(multi 56 1)
;(+ 56 (multi 56 (- 1 1)))
;(+ 56 (multi 56 0))
;(+ 56 0)
;56

;(multi 7 7)
;(+ 7 (multi 7 6))
;(+ 7 (multi 14 3))
;(+ 7 (+ 14 (multi 14 2)))
;(+ 7 (+ 14 (multi 28 1)))
;(+ 7 (+ 14 (+ 28 0)))
;49
;再帰的