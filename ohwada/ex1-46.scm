; iterative-improve
(define (iterative-improve f g)
  (lambda (guess)
    (define (iter x)
      (if (f x)
          x
          (iter (g x))))
    (iter guess)))


; sqrt

(define (sqrt-iter x first-guess)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (test guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve test improve) first-guess))

(define (sqrt x)
  (sqrt-iter x 1.0))

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))


; テスト
(print (sqrt 9))




; fixed-point
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (improve guess)
    (f guess))
  (define (test guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve test improve) first-guess))


; テスト
(print (fixed-point cos 1.0))
