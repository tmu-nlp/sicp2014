;sec2_1_3.scm

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)


(define (car z) (z 0))


(define (cdr z) (z 1))



(define a (cons 3 5))
(car a)
(cdr a)