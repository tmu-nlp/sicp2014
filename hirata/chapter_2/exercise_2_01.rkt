(define (absolute x)
  (if (> n 0)
      n
      (-n)))

(define (make-rat n d)
  (let* ((g (gcd n d))
    (n1 (/ n g))
    (d1 (/ d g)))
    (if (< d1 0)
        (cons (* -1 n1) (* -1 d1))
        (cons n1 d1))
    ))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;(define one-half (make-rat -2 6))
(print-rat (make-rat 1 2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))