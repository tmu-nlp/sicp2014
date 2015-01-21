;ex2_10.scm
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;Alyssa
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define p (make-interval -2 8))
(define q (make-interval -4 10))

(div-interval p q)

;new
(define (new-div-interval x y)
  (let ((yl (lower-bound y))
        (yu (upper-bound y)))
    (if (and (<= yl 0) (<= 0 yu))
        (error "error" yl yu)
        (mul-interval x
                          (make-interval (/ 1.0 yu) (/ 1.0 yl))))))

(new-div-interval p q)
