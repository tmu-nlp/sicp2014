#!/usr/bin/gosh

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;ex2_59のunion
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
;        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                   (cond ((= x1 x2)
                          (cons x1 (union-set (cdr set1) (cdr set2))))
                         ((< x1 x2)
                          (cons x1 (union-set (cdr set1) set2)))
                         ((< x2 x1)
                          (cons x2 (union-set set1 (cdr set2)))))))))

#?=(union-set '(1 2 3 4 5) '(2 4 6))
#?=(union-set '(1 2) '(2 4 6))
