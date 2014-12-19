#!/usr/bin/gosh
;;execution　gosh Exercise2-6.scm

(define (inc x) (+ x 1))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(print "(define one (lambda (f) (lambda (x) (f x))))")
(define two (lambda (f) (lambda (x) (f (f x)))))
(print "(define two (lambda (f) (lambda (x) (f (f x)))))")
(define three (lambda (f) (lambda (x) (f (f (f x))))))
(print "(define three (lambda (f) (lambda (x) (f (f (f x))))))")

(define (add n1 n2)
  (lambda (f)
    (lambda (x)
      ((n1 f) ((n2 f) x)))))
(print )
(print "(((add two three) inc) 0)")
(print (((add two three) inc) 0))
