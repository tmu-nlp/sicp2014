#!/usr/bin/goshd
;;execution　gosh Exercise2-2.scm

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (avg a b) (/ (+ a b) 2))
(define (midpoint-segment seg)
  (cons
   (avg (x-point (start-segment seg))
        (x-point (end-segment seg)))
   (avg (y-point (start-segment seg))
        (y-point (end-segment seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define point1 (make-point 1 1))
(print-point point1)
(define point2 (make-point 3 3))
(print-point point2)
(define segment (make-segment point1 point2))
(print )
(print "make midpoint")
(print-point (midpoint-segment segment))
(print )
