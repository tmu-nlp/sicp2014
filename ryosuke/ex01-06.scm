;; cond is applicative-order evaluation.
;; so sqrt-iter is recursive called infinitely
(define (new-if predicate then-clause else-caluse)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- ( square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))

(print (sqrt 9))
