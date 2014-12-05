(define (average-damp f) (lambda (x) (average x (f x))))
(define (average x y) (/ (+ x y) 2))

(define (n-root x n c)
    (fixed-point 
        ((repeated average-damp c) (lambda (y) (/ x (expt y (- n 1)))))
        1.0))

(print (n-root (expt 2 3) 3 1))
(print (n-root (expt 2 4) 4 2))
(print (n-root (expt 2 5) 5 2))
(print (n-root (expt 2 8) 8 3))
(print (n-root (expt 2 9) 9 3))
(print (n-root (expt 2 16) 16 4))

(define (nth-root x n)
    (let ((c (round (/ (log n) (log 2)))))
        (fixed-point
            ((repeated average-damp c) (lambda (y) (/ x (expt y (- n 1)))))
            1.0)))

(print (nth-root (expt 2 50) 50))
