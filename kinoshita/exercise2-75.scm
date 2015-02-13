(define (make-from-mag-ang r a)
    (define (dispatch op)
        (cond ((eq? op 'real-part) (* r (cos a)))
              ((eq? op 'imag-part) (* r (sin a)))
              ((eq? op 'magnitude) r)
              ((eq? op 'angle) a)
              (else
                (error "Unknown op -- MAKE-FROM-MAG-ANG:" op))))
    dispatch)

(define pi 3.1415926)
(define z (make-from-mag-ang 1.0 (/ pi 6)));

(print (z 'real-part))
(print (z 'imag-part))
