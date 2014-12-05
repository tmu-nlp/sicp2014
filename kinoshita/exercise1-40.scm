(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))
(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(print (newtons-method (cubic 1 1 1) 1))
