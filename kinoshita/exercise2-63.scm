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
                                (copy-to-list
                                    (right-branch tree)
                                    result-list)))))
    (copy-to-list tree '()))

(define tree1 
    (adjoin-set 11
        (adjoin-set 5
            (adjoin-set 1
                (adjoin-set 9
                    (adjoin-set 3
                        (adjoin-set 7 '())))))))
(define tree2
    (adjoin-set 11
        (adjoin-set 9
            (adjoin-set 5
                (adjoin-set 7
                    (adjoin-set 1
                        (adjoin-set 3 '())))))))

(define tree3
    (adjoin-set 11
        (adjoin-set 7
            (adjoin-set 1
                (adjoin-set 9
                    (adjoin-set 3
                        (adjoin-set 5 '())))))))

;a
(print (tree->list-1 tree1))
(print (tree->list-1 tree2))
(print (tree->list-1 tree3))
(print (tree->list-2 tree1))
(print (tree->list-2 tree2))
(print (tree->list-2 tree3))

;b
;tree->list-1が再帰プロセス
;tree->list-2が反復プロセス
;よってtree->list2のほうがわずかにステップ数が少ない
