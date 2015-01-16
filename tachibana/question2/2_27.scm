(define (reverse items)
  (define (iter rev rest)
    (if (null? rest)
        rev
        (iter (cons (car rest) rev) (cdr rest))))

  (iter (list) items)
)

(define (deep-reverse lists)
  (define (iter rev rest)
    (if (null? rest)
        rev
        (if (list? (car rest))
        	(iter (cons (iter (list) (car rest)) rev) (cdr rest))
        (iter (cons (car rest) rev) (cdr rest)))))

  (iter (list) lists)
)

(print "(reverse (list (list 1 2) (list 3 4) (list 5 6)))")
(print (reverse (list (list 1 2) (list 3 4) (list 5 6))))

(print "(deep-reverse (list (list 1 2) (list 3 4) (list 5 6)))")
(print (deep-reverse (list (list 1 2) (list 3 4) (list 5 6))))
