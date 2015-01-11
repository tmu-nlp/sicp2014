;ex2_20.scm

(define (same-parity first . rest)
  (define (itr matched? l1)
    (cond ((null? l1) nil)
          ((matched? (car l1))
           (cons (car l1) (itr matched? (cdr l1))))
          (else (itr matched? (cdr l1)))))
  (itr (if (= (remainder first 2) 0)
                 (lambda (x) (= (remainder x 2) 0))
                 (lambda (x) (= (remainder x 2) 1)))
                 (cons first rest)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)