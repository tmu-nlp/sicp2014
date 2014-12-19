#!/usr/bin/gosh
;;execution gosh ex02-10.scm

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* (/ c 100.0) p))
                 (+ c (* (/ c 100.0) p))))
(define (percent i)
  (* (/ (width i) (center i)) 100.0))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(print "(make-center-percent 100 15)")
(print  (make-center-percent 100 15))
(print "(percent (make-center-percent 100 15))")
(print (percent (make-center-percent 100 15)))


