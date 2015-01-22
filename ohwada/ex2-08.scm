(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))


(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))


(print (sub-interval
         (make-interval 4 6)
         (make-interval 5 9))) ; (-5, 1) 4-9=-5, 6-5=1
