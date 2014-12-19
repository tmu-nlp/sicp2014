(use srfi-27)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2.0))
(define (inc x)
  (+ x 1))

(define tolerance 0.000001)
(define (itetative-improve check next)
  (lambda (x) (if (check x) x ((itetative-improve check next) (next x)))))

(define (sqrt-check? x)
  (lambda (y) (and (> y 0)
				   (<= (abs (- x (square y))) tolerance))))
(define (sqrt-next x)
  (lambda (y) (average y (/ x y))))
(define (sqrt x)
  ((itetative-improve (sqrt-check? x) (sqrt-next x)) 1.0))

(define (fixed-check? f) (lambda (x)(< (abs (- x (f x))) tolerance)))
(define (fixed-point f first-guess)
  ((itetative-improve (fixed-check? f) f) first-guess))

(print (sqrt 2))
(print (sqrt 3))
(print (sqrt 4))

(print (fixed-point cos 1.0))
(print (fixed-point (lambda (y) (average y (/ 4 y))) 1.0))
