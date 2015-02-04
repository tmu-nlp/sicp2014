;ex2_61.scm

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set '1 '(2 3 4))
(adjoin-set '4 '(2 3 5))
(adjoin-set '5 '(2 3 4))