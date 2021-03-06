#!/usr/bin/gosh
;;execution gosh ex01-39.scm

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

(print "(tan-cf 3.14 100)")
(print (tan-cf 3.14 100))
(print "(tan-cf (/ 3.14 4) 100)")
(print (tan-cf (/ 3.14 4) 100))

