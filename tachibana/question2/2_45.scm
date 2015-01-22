(define wave einstein)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (split func1 func2)
  (lambda (painter n)
    (let ((smaller (if (= n 0) painter ((split func1 func2) painter (- n 1)))))
      (func1 painter (func2 smaller smaller)))))

(define right-split (split beside below))
(define up-split (split below beside))