(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(define (reverse list1)
    (define (reverse-iter list2 result)
        (if (null? list2)
            result 
            (reverse-iter (cdr list2) (cons (car list2) result))))
    (reverse-iter list1 ()));

(print (reverse (list 1 4 9 16 25)))
