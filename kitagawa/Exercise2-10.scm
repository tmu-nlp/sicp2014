#!/usr/bin/gosh
;;execution gosh Exercise2-10.scm

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;;before
(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;;after
(define (new-div-interval x y)
  (let ((yl (lower-bound y))
        (yu (upper-bound y)))
    (if (or (<= yu 0) (= 0 yl))
        (error "error" yl yu)
        (mul-interval x
                          (make-interval (/ 1.0 yu) (/ 1.0 yl))))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define r1 (make-interval 6 8))
(print "(define r1 (make-interval 6 8))")
(define r2 (make-interval 7 9))
(print "(define r2 (make-interval 7 9))")

(print "(new-div-interval r1 r2)")
(print (new-div-interval r1 r2))
(print )

(define r3 (make-interval -2 8))
(print "(define r3 (make-interval -2 8))")
(define r4 (make-interval -4 10))
(print "(define r4 (make-interval -4 10))")

(print "(new-div-interval r3 r4)")
(print (new-div-interval r3 r4))
(print )


