#lang racket

; 重複を許すリスト形式の集合の操作


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))


(element-of-set? 3 '(2 3 2 1 3 2 2))
(adjoin-set 2 '(2 3 2 1 3 2 2))
(intersection-set '(2 3 2 5) '(2 3 2 1))
(union-set '(2 3 2) '(2 3 2))
