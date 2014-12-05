;ex1_33.scm
(define (square a) (* a a))
;filtered-accumulate
(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner filter null-value term (next a) next b))
          (combiner null-value (filtered-accumulate combiner filter null-value term (next a) next b)))))

;(prime?)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;a.sum of (primes between "a" and "b" )^2 
(define (sum-squares-primes a b)
  (define (combiner a b) (+ a b))
  (define (filter a) (prime? a))
  (define (inc x) (+ x 1))
  (filtered-accumulate combiner filter 0 square a inc b))

(sum-squares-primes 1 3)