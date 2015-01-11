; reverse

(define (reverse items)
  (define (reverse-iter x y)
    (if (null? x)
        y
        (reverse-iter (cdr x) (cons (car x) y))))
  (reverse-iter items ()))



(print (reverse (list 1 4 9 16 25)))
