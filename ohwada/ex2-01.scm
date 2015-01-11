(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; 負の数を引数にとることのできる make-rat
(define (make-rat n d) 
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (< d 0)) (cons (/ (abs n) g) (/ (abs d) g)))
          ((or (< n 0) (< d 0)) (cons (/ (- (abs n)) g) (/ (abs d) g)))
          (else (cons (/ n g) (/ d g))))))



(print-rat (make-rat 1 3)) ; 1/3
(print-rat (make-rat -1 -3)) ; 1/3
(print-rat (make-rat -1 3)) ; -1/3
(print-rat (make-rat 1 -3)) ; -1/3
