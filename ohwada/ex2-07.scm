(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))


(print (upper-bound (make-interval 2 8)))
(print (lower-bound (make-interval 2 8)))
