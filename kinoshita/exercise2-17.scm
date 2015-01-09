(define (last-pair list1)
    (define (last-iter a)
        (if (null? (cdr a))
            (list (car a))
            (last-iter (cdr a))))
    (last-iter list1))

(print (last-pair (list 23 72 149 34)))
