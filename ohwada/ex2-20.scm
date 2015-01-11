(define (same-parity first . ints)
  (define (judge int)
    (if (even? first) (even? int)
        (odd? int)))
  (define (same-parity-iter x y)
    (cond ((null? x) y)
          ((judge (car x)) (cons (car x) (same-parity-iter (cdr x) y)))
          (else (same-parity-iter (cdr x) y))))
  (same-parity-iter (cons first ints) ()))


(print (same-parity 1 2 3 4 5 6 7))
(print (same-parity 2 3 4 5 6 7))
