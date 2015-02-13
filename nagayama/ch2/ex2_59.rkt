#lang racket

; 重複を許すリスト形式の集合の操作

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (union-set (adjoin-set (car set2) set1) (cdr set2))))

(union-set '(1 2 3) '(2 4 6))
(union-set '(1 2 3) '(1 2 3))
(union-set '(1 2 3) '(4 5 6))