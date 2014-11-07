;ex_1_12.scm
;use bionomial theorem
;(a,b) = (row, col)
;(a,b) = (a-1)C(b-1)
;nCm = (
;4C3 = (4/3)(3/2)(2/1)

(define (combi p q)
    (cond ((= q 1) p)
          (else (* (/ p q) (combi (- p 1) (- q 1))))))

(define (pas-tri n m)
  (combi (- n 1) (- m 1)))
  