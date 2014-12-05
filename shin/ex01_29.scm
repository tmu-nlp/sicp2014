#!/usr/bin/gosh

(define (cube x) (* x x x))
(define (next i) (+ i 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;----------------------------------------------------------
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (coef x)
    (cond ((or (= x 0) (= x n)) 1)
          ((even? x) 2)
          (else 4)))
  (define (term k) (* (coef k) (f (+ a (* k h)))))
  (* (/ h 3) (sum term 0 next n)))
;;----------------------------------------------------------
#?=(integral cube 0 1 0.01)
#?=(integral cube 0 1 0.001)
#?=(simpson cube 0 1 100)
#?=(simpson cube 0 1 1000)