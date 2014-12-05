(define (iterative-improve enough? method)
    (lambda (first-guess)
        (define (iter guess)
            (if (enough? guess)
                guess
                (iter (method guess))))
        (iter first-guess)))

(define (sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (average guess (/ x guess)))
    ((iterative-improve good-enough? improve) 1.0))

(define (average x y)
    (/ (+ x y) 2))

(define (fixed-point f first-guess)
    (define (close-enough? guess)
        (< (abs (- guess (f guess)))
            0.00001))
    ((iterative-improve close-enough? f) first-guess))
