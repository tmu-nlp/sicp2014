#lang racket
(define (element-of-set? x set) ;O(n)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;(element-of-set? 2 '(1 3 2))

(define (adjoin-set x set) ;O(1)
  (cons x set))
;(adjoin-set 2 '(1 3 2))

(define (union-set set1 set2) ;O(n)
  (if (null? set2)
      set1
      (adjoin-set (car set2) 
                  (union-set (cdr set2) set1))))
;(union-set '(2 1) '(1 2 3))
(define (union-set2 set1 set2) ;O(1)
  (append set1 set2))
(union-set2 '(2 1) '(1 2 3))

(define (intersection-set set1 set2) ;O(n^2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
(intersection-set '(1 3 3 2) '(3))