(define right-split (split beside below))

(define up-split (split below beside))

(define (split ope1 ope2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split ope1 ope2) painter (- n 1))))
          (ope1 painter (ope2 smaller smaller))))))
