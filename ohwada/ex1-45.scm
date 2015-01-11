; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; average-damp
(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))


; repeated
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))





; n 乗根を計算する手続き
(define (n-rt x n) 
  (define (f x n)
    (if (= n 1)
        x
        (* x (f x (- n 1)))))
  (define i 2)
  (fixed-point ((repeated average-damp i) (lambda (y) (/ x (f y (- n 1)))))
               1.0))

(print (n-rt 8 4))
