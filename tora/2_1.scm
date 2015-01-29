#lang racket
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define (make-rat2 n d)
    (if (< d 0)
        (make-rat (- n) (- d))
        (make-rat n d)))

(print-rat (make-rat2 2 4))
(print-rat (make-rat2 -2 6))
(print-rat (make-rat2 2 -8))