;;execution gosh Exercise1-36.scm

(define tolerance 0.00001)

;;not using average
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (f x)
  (/ (log 1000) (log x)))


(fixed-point f 100)

;;using average
(define (average x y)
  (/ (+ x y) 2.0))

(fixed-point
 (lambda (x) (average x (f x))) 100)
