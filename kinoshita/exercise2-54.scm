(define (equal? a b)
    (if (and (pair? a) (pair? b))
        (and (equal? (car a) (car b))
             (equal? (cdr a) (cdr b)))
        (eq? a b)))

(print (equal? 'a 'b))
(print (equal? 'a 'a))
(print (equal? '(a b c) '(a b c)))
(print (equal? '(a b c) '(b b c)))
(print (equal? 1 1))

