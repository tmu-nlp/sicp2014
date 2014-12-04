(define (compose f g)
    (lambda (x) (f (g x))))
(define (repeated f n)
    (define (iter i result)
        (if (> i n)
            result
            (iter (+ 1 i) (compose result f))))
    (iter 1 (lambda (x) x)))

(print ((repeated (lambda (x) (* x x)) 2) 5))
