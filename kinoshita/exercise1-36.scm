(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
    (define (try guess)
        (display guess)
        (newline)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define (calc guess)
    (fixed-point (lambda (x) (/ (log 1000) (log x))) guess))

(define (calc2 guess)
    (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) guess))

(print (calc 1.1))
(display "***")
(newline)
(print (calc2 1.1))
