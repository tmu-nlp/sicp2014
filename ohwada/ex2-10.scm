(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


; div-interval

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(print (div-interval (make-interval 2 4) (make-interval -2 3)))



(define (div-interval x y)
  (let ((a (lower-bound y))
        (b (upper-bound y)))
  (if (< (* a b) 0)
      (display "Divided by the interval that spans zero.\n")
      (mul-interval x
                    (make-interval (/ 1.0 a)
                                   (/ 1.0 b))))))

(print (div-interval (make-interval 2 4) (make-interval -2 3)))
