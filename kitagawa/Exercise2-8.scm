#!/usr/bin/gosh
;;execution gosh Exercise2-8.scm

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define r1 (make-interval 6 8))
(print "(define r1 (make-interval 6 8))")
(define r2 (make-interval 7 9))
(print "(define r2 (make-interval 7 9))")

(print "(sub-interval r1 r2)")
(print (sub-interval r1 r2))
(print )


