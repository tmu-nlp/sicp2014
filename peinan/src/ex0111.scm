#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (f n)
  (if (< n 3) n
    (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))


(define (ff n)
  (define (iter new old old2 count)
    (if (>= count n) new
      (iter (+ new old old2) new old (+ 1 count))))
    (iter 3 2 1 3))

#?=(f 10)
#?=(ff 10)
