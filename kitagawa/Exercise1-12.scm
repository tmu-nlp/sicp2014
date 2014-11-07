(define (pascal x y) ;x:行, y:列
    (cond ((= y 1) 1)
          ((= x y) 1)
          (else (+ (pascal (- x 1) (- y 1))
                   (pascal (- x 1) y)))))

(pascal 6 3)

