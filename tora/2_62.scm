#lang racket
(define (adjoin-set x set)
  (cond ((null? set)
         (cons x set))
        ((< x (car set))
         (cons x set))
        (else
         (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2) 
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x (car set1)) (y (car set2)))
           (cond
             ((= x y)
              (cons x (union-set (cdr set1) (cdr set2))))
             ((< x y)
              (cons x (union-set (cdr set1) set2)))
             ((> x y)
              (cons y (union-set set1 (cdr set2)))))))))
(union-set '(1 3 5) '(1 2 3))