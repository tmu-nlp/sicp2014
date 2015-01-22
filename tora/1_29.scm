#lang racket
(define (sum term a next b)
  (if (> a b)
  0
  (+ (term a)
     (sum term (next a) next b))))

(define (simpson f a b n)

  (define h (/ (- b a) n))

  (define (y k) (f (+ a (* k h))))

  (define (factor k)
    (cond ((or (= k 0)(= k n))
           1)
          ((odd? k)
           4)
          (else 
            2)))

  (define (inc k) (+ k 1))

  (define (term k)
    (* (factor k) (y k)))

  (* (sum term 0 inc n )
     (/ h 3)))

(define (cube x) (* x (* x x)))

(simpson cube 0 1 1000)