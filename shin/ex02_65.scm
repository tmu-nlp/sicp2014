#!/usr/bin/gosh

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set s1 s2)
  (cond 
    ((and (null? s1) (null? s2)) '())
    ((null? s1) s2)
    ((null? s2) s1)
    ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
    ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))
    (else (cons (car s1) (union-set (cdr s1) (cdr s2))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
    (copy-to-list tree '()))

(define (list->tree elements) 
  (car (partial-tree elements (length elements)))) 
 
(define (partial-tree elts n) 
  (if (= n 0) 
      (cons '() elts) 
      (let ((left-size (quotient (- n 1) 2))) 
        (let ((left-result (partial-tree elts left-size))) 
          (let ((left-tree (car left-result)) 
                (non-left-elts (cdr left-result)) 
                (right-size (- n (+ left-size 1)))) 
            (let ((this-entry (car non-left-elts)) 
                  (right-result (partial-tree (cdr non-left-elts) 
                                              right-size))) 
              (let ((right-tree (car right-result)) 
                    (remaining-elts (cdr right-result))) 
                (cons (make-tree this-entry left-tree right-tree) 
                      remaining-elts))))))))


(define (union-tree tree1 tree2)
  (let ((s1 (tree->list tree1))
        (s2 (tree->list tree2)))
    (list->tree (union-set s1 s2))))

(define (intersection-tree tree1 tree2)
  (let ((s1 (tree->list tree1))
        (s2 (tree->list tree2)))
    (list->tree (intersection-set s1 s2))))

(define tree1 (list->tree '(1 2 3 4 5)))
(define tree2 (list->tree '(2 4 6)))

#?=tree1
#?=tree2
#?=(union-tree tree1 tree2)
#?=(intersection-tree tree1 tree2)
