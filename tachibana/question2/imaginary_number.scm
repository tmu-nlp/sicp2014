(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z) (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))



