#!/usr/bin/gosh
(load "./ex02_56.scm")

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))


#?=(deriv '(* x y (+ x 3)) 'x)
