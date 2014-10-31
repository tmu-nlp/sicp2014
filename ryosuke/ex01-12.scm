(define (pascal h w)
  (cond ((= h w) 1)
        ((= w 1) 1)
        (else (+ (pascal (- h 1) (- w 1))
                 (pascal (- h 1) w)))))


