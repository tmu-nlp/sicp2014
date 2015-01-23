#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (key p) (car p))
(define (value p) (cdr p))
(define (make-pair key value)
  (list key value))

(define (lookup given-key tree)
  (if (null? tree) false
      (let ((entry-key (key (entry tree))))
        (cond ((= entry-key given-key) (value (entry tree)))
              ((< entry-key given-key) (lookup given-key (right-branch tree)))
              ((> entry-key given-key) (lookup given-key (left-branch tree)))))))

(define tree3
  (make-tree (make-pair 5 'five)
             (make-tree (make-pair 3 'three)
                        (make-tree (make-pair 1 'one) null null)
                        null)
             (make-tree (make-pair 9 'nine)
                        (make-tree (make-pair 7 'seven) null null)
                        (make-tree (make-pair 11 'eleven) null null))))

(lookup 1 tree3)
(lookup 2 tree3)
(lookup 3 tree3)
(lookup 4 tree3)
(lookup 5 tree3)
(lookup 6 tree3)
(lookup 7 tree3)
(lookup 8 tree3)
(lookup 9 tree3)
(lookup 10 tree3)
(lookup 11 tree3)
