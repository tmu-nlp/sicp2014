;ex2_54.scm

(define (equal? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2))
        ((and (pair? list1) (pair? list2) (equal? (cdr list1) (cdr list2)) (eq? (car list1) (car list2))))
        (else #f)))


(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))