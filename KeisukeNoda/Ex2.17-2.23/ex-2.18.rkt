#lang racket

;; 引数としてリストをとり、同じ要素の逆順のリストを返す

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items null))

(define (reverse-r items)
  (if (null? items)
      null
      (append (reverse-r (cdr items)) (cons (car items) null))))

(list 1 4 9 16 25)
(reverse (list 1 4 9 16 25))   ;-> (25 16 9 4 1)
(reverse-r (list 1 4 9 16 25)) ;-> (25 16 9 4 1)