(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))




(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define treeA (make-tree 7
                         (make-tree 3
                                    (make-tree 1 () ())
                                    (make-tree 5 () ()))
                         (make-tree 9
                                    ()
                                    (make-tree 11 () ())))) 
;(print treeA)
;(print (tree->list-1 treeA))

(define treeB (make-tree 3
                         (make-tree 1 () ())
                         (make-tree 7
                                    (make-tree 5 () ())
                                    (make-tree 9
                                               ()
                                               (make-tree 11 () ())))))
;(print treeB)


(define treeC (make-tree 5
                         (make-tree 3
                                    (make-tree 1 () ())
                                    ())
                         (make-tree 9
                                    (make-tree 7 () ())
                                    (make-tree 11 () ()))))
;(print treeC)


(define treeD (make-tree 4
                         (make-tree 3
                                    (make-tree 2 () ())
                                    ())
                         (make-tree 10
                                    (make-tree 6 () ())
                                    (make-tree 11 () ()))))
;(print treeD)
(print (tree->list-1 treeD))


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




; 2.65

; union-set
(define (union-set tree1 tree2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else
            (let ((x1 (car list1)) (x2 (car list2)))
              (cond ((< x1 x2)
                     (cons (car list1) (union-list (cdr list1) list2)))
                    ((> x1 x2)
                     (cons (car list2) (union-list list1 (cdr list2))))
                    ((= x1 x2)
                     (cons (car list1) (union-list (cdr list1) (cdr list2)))))))))
  (let ((A (tree->list-1 tree1))
        (B (tree->list-1 tree2)))
    (union-list A B)))
;    (list->tree (union-list A B))))


(print (union-set treeA treeD)) 


; intersection-set
(define (intersection-set tree1 tree2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
        ()
        (let ((x1 (car list1)) (x2 (car list2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-list (cdr list1)
                                             (cdr list2))))
                ((< x1 x2)
                 (intersection-list (cdr list1) list2))
                ((< x2 x1)
                 (intersection-list list1 (cdr list2)))))))
  (let ((A (tree->list-1 tree1))
        (B (tree->list-1 tree2)))
    (intersection-list A B)))
;    (list->tree (union-list A B))))


(print (intersection-set treeA treeD))
