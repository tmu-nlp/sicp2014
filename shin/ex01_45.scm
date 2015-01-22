#!/usr/bin/gosh
(load "./ex01_43.scm")
(load "./ex01_36.scm")
;;43---------------------------------------------
;(define (square x) (* x x))
;(define (compose f g)
;  (lambda (x) (f (g x))));
;
;(define (repeated f n)
;  (if (= n 0)
;      (lambda (x) x)
;      (compose f (repeated f (- n 1)))))
;;---------------------------------------------
;;35--------------------------
;(define tolerance 0.00001)
;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))
;;----------------------------

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;----------------------------------------------------------
(define (n-fold-average-damp n)
  (repeated average-damp n))

(define (n-root-exp n x m)
  (fixed-point ((n-fold-average-damp m)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;;----------------------------------------------------------
;#?=(n-root-exp 2 2 0) ;==> NG
#?=(n-root-exp 2 2 1)
#?=(n-root-exp 3 2 1)
;(n-root-exp 4 2 1) ==> 収束しない
#?=(n-root-exp 3 2 2)
;(n-root-exp 7 2 2)
;(n-root-exp 8 2 2) ==> 収束しない
#?=(n-root-exp 3 2 3)
#?=(n-root-exp 8 2 3)
;;n=log2(m)の少数切り捨て


;;----------------------------------------------------------
(define (n-root n x)
  (define (damp-count m)
    (if (< m 2)
        0
        (+ 1 (damp-count (/ m 2)))))
  (fixed-point ((n-fold-average-damp (damp-count n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))
;;-------------------------
#?=(n-root 8 2)