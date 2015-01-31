(define (equal? list1 list2)
  (define (iter x y)
    (cond ((and (null? x) (null? y)) #t)
          ((or (not (pair? x)) (not (pair? y))) #f)
          ((eq? (car x) (car y)) (iter (cdr x) (cdr y)))
          (else #f)))
    (iter list1 list2))


(print (equal? '(this is a list) '(this is a list))) ; #t
(print (equal? '(this is a list) '(this (is a) list))) ; #f
(print (equal? '(this is a list) '(this is a))) ; #f
