(define (reverse list1)
    (define (reverse-iter list2 result)
        (if (null? list2)
            result 
            (reverse-iter (cdr list2) (cons (car list2) result))))
    (reverse-iter list1 ()));

(define (deep-reverse list1)
    (if (pair? list1)
        (cond
            ((null? (cdr list1)) (deep-reverse (car list1)))
            ((null? (car list1)) (deep-reverse (cdr list1)))
            (else
                (list
                    (deep-reverse (cdr list1))
                    (deep-reverse (car list1)))))
        list1))

(define x (list (list 1 2) (list 3 4)))
(print x)
(print (reverse x))
(print (deep-reverse x))
