#lang racket

(require "public.rkt")

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))


(reverse (list 1 4 9 16 25))
'(25 16 9 4 1)

(reverse2 (list 1 4 9 16 25))
'(25 16 9 4 1)