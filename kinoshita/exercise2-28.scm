(define (fringe list1)
    (if (pair? list1)
        (cond
            ((null? (car list1)) (fringe (cdr list1)))
            ((null? (cdr list1)) (fringe (car list1)))
            (else
                (append (fringe (car list1)) (fringe (cdr list1)))))
        (list list1)))

(define x (list (list 1 2) (list 3 4)))
(print (fringe x))
(print (fringe (list x x)))
