; Newton 法
(define (newton-trainsform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newtons-method g guess)
  (fixed-point (newton-trainsform g) guess))


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


; x^3 + ax^2 + bx + c の零点を近似する cubic
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(print (newtons-method (cubic 2 3 4) 1))

