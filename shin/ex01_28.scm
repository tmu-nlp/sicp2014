#!/usr/bin/gosh

(define (mod a b) (remainder a b))

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;(if (and (< 1 tmp) (< tmp m) (= (mod (square tmp) m) 1))
         ;      ここ問題
        (else (mod (* base (expmod2 base (- exp 1) m)) m))))

(define (miller-rabin-test? n)
  (define (tt a i)
    (= ((expmod2 a n n) a) 
  (tt (+ 1 )))

(define (fermat-test2 n)
  (define (try-it a)
    (= (expmod2 a n n) a))
    (try-it (+ 1 (random(- n 1)))))
(define (fast-prime2? n times)
  (cond ((= times 0) #t)
        ((fermat-test2 n) (fast-prime2? n (- times 1)))
        (else #f)))
