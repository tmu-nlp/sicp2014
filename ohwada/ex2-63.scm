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


; a.
; tree->list-1 と tree->list-2 はどんな木に対しても同じ結果を生じる


(define treeA (make-tree 7
                         (make-tree 3
                                    (make-tree 1 () ())
                                    (make-tree 5 () ()))
                         (make-tree 9
                                    ()
                                    (make-tree 11 () ())))) 
(print treeA)

(print (tree->list-1 treeA))
(print (tree->list-2 treeA))


(define treeB (make-tree 3
                         (make-tree 1 () ())
                         (make-tree 7
                                    (make-tree 5 () ())
                                    (make-tree 9
                                               ()
                                               (make-tree 11 () ())))))
(print treeB)

(print (tree->list-1 treeB))
(print (tree->list-2 treeB))


(define treeC (make-tree 5
                         (make-tree 3
                                    (make-tree 1 () ())
                                    ())
                         (make-tree 9
                                    (make-tree 7 () ())
                                    (make-tree 11 () ()))))
(print treeC)

(print (tree->list-1 treeC))
(print (tree->list-2 treeC))



; b.
; test->list-2 の方が増加が遅い？
; 理由: test->list-2 は反復的だから
