(define (compose f g)
    (lambda (x) (f (g x))))
(define (repeated f n)
    (define (iter i result)
        (if (> i n)
            result
            (iter (+ 1 i) (compose result f))))
    (iter 1 (lambda (x) x)))

(define dx 0.00001)

(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth f n)
    ((repeated smooth n) f))

(n-smooth (lambda (x) (* x x)) 3)
