#!/usr/bin/gosh
(load "./ex01_21.scm")
;prime?

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;----------------------------------------------------------
;; filtered-accumulate
;;再帰プロセス
(define (filtered-accumulate-rec combiner null-value term filter a next b)
  (cond ((> a b) null-value)
        ((filter a)
          (combiner
            (term a)
            (filtered-accumulate-rec
              combiner null-value term filter (next a) next b)))
        (else
          (filtered-accumulate-rec
            combiner null-value term filter (next a) next b))))

;;反復プロセス
(define (filtered-accumulate-iter combiner null-value term filter a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (filtered-accumulate-iter 
          combiner (combiner null-value (term a)) term filter (next a) next b))
        (else 
         (filtered-accumulate-iter
          combiner null-value term filter (next a) next b))))

(define filtered-accumulate filtered-accumulate-rec)
;(define filtered-accumulate filtered-accumulate-iter)

;;----------------------------------------------------------
;; a.
(define (sum-of-square-primes a b)
  (filtered-accumulate + 0 square prime? a inc b))

;; b.
(define (product-of-irreducibles n)
  (define (primeWith? n)
    (lambda (x) (= (gcd n x) 1)))
  (filtered-accumulate * 1 identity (primeWith? n) 1 inc (- n 1)))

#?=(sum-of-square-primes 2 5)
#?=(product-of-irreducibles 10)