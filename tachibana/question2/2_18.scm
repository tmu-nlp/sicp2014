(define (reverse items)
  (define (iter rev rest)
    (if (null? rest)
        rev
        (iter (cons (car rest) rev) (cdr rest))))
  (iter (list) items))

(print "(reverse (list 1 4 9 16 25))")
(print (reverse (list 1 4 9 16 25)))
