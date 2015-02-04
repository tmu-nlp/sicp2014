#!/usr/bin/gosh

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (key p) (car p))
(define (value p) (cdr p))
(define (make-pair key value) (list key value))

(define (lookup given-key tree)
  (if (null? tree) #f
      (let ((entry-key (key (entry tree))))
        (cond ((= entry-key given-key) (value (entry tree)))
              ((< entry-key given-key) (lookup given-key (right-branch tree)))
              ((> entry-key given-key) (lookup given-key (left-branch tree)))))))

;(define tree3
;  (make-tree 5
;             (make-tree 3
;                        (make-tree 1 '() '())
;                        '())
;             (make-tree 9
;                        (make-tree 7 '() '())
;                        (make-tree 11 '() '()))))

(define tree3
  (make-tree (make-pair 5 'five)
             (make-tree (make-pair 3 'three)
                        (make-tree (make-pair 1 'one) '() '())
                        '())
             (make-tree (make-pair 9 'nine)
                        (make-tree (make-pair 7 'seven) '() '())
                        (make-tree (make-pair 11 'eleven) '() '()))))

#?=(lookup 1 tree3)
#?=(lookup 2 tree3)
#?=(lookup 3 tree3)