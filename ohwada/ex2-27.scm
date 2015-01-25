(define x (list (list 1 2) (list 3 4)))


(define (deep-reverse items)
  (define (deep-reverse-iter x y)
    (cond ((null? x) y)
          ((pair? (car x))
           (deep-reverse-iter (cdr x) (cons (deep-reverse-iter (car x) ()) y)))
          (else (deep-reverse-iter (cdr x) (cons (car x) y)))))
  (deep-reverse-iter items ()))



(print (reverse x)) ; ((3 4) (1 2))
(print (deep-reverse x)) ; ((4 3) (2 1))

