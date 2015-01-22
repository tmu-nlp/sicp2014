#!/usr/bin/gosh
; -*- coding:utf-8 -*-

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
     (if (< d 0)
         (cons (/ (- n) g) (/ (- d) g))
         (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))


#?=(print-rat (make-rat 3 6))
#?=(print-rat (make-rat -3 6))
#?=(print-rat (make-rat 3 -6))
#?=(print-rat (make-rat -3 -6))
