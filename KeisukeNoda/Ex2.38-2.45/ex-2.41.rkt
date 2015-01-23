#lang racket

(require "public.rkt")

(define (unique-triples n) 
  (flatmap (lambda (i) 
             (flatmap (lambda (j) 
                    (map (lambda (k) 
                           (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (n-triples-are-sum n s)
  (filter (lambda (pair)
            (= s (+ (car pair) (cadr pair) (caddr pair))))
          (unique-triples n)))

(unique-triples 10)

(n-triples-are-sum 10 10)