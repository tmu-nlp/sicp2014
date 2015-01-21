(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
    (if (or (<= (upper-bound y) 0) (= (lower-bound y) 0))
        -1
        (mul-interval
            x
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
    (add-interval
        x
        (make-interval (* -1.0 (upper-bound y))
                        (* -1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound intr)
    (cdr intr))

(define (lower-bound intr)
    (car intr))

(define intr1 (make-interval 2 7))
(define intr2 (make-interval 3 3))

(print (div-interval intr1 intr2))
