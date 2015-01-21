;#lang racket
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (* (term a) result))))
    (iter a 1))
(define (inc x) (+ x 1))

(define (pi-product n)
  (* 4 (/ (product item-numer 1 inc n)
          (product item-deno 1 inc n))))

(define (item-numer i)
  (cond ((= i 1) 2)
        ((even? i) (+ i 2))
        (else (+ i 1))))

(define (item-deno i)
  (cond ((even? i) (+ i 1))
        (else (+ i 2))))

(pi-product 100)
