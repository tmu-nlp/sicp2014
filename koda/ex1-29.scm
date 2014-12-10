(use srfi-27)

(define (cube x)
  (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (* (/ h 3) (+ (f a) (f (+ a (* n h)))(simpson-iter f a h (- n 1)))))
(define (simpson-iter f a h k)
  (define y (f (+ a (* k h))))
  (define c (if (even? k) 2 4))
  (if (> k 1) (+ (* c y) (simpson-iter f a h (- k 1)))) 0 )

(print (simpson cube 0 1 100))

