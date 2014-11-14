#!/usr/bin/gosh

;;17の反復プロセスver.
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multi-iter n a b)
  (cond ((= b 0) n)
        ((even? b) (multi-iter n (double a) (halve b)))
        (else (multi-iter (+ n a) a (- b 1)))))

(define (multi a b)
  (multi-iter 0 a b))

(multi 7 8)
;; => 56

;(multi 7 8)
;(multi-iter 0 7 8)
;(multi-iter 0 (double 7) (halve 8))
;(multi-iter 0 (+ 7 7) (/ 8 2))
;(multi-iter 0 14 4)
;(multi-iter 0 28 2)
;(multi-iter 0 56 1)
;(multi-iter (+ 0 56) 56 (- 1 1))
;(multi-iter 56 56 0)
;56

;(multi 7 3)
;(multi-iter 0 7 3)
;(multi-iter 7 7 2)
;(multi-iter 7 14 1)
;(multi-iter 21 14 0)
;21