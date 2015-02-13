(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


(define key-tree  (make-tree 3
                            (make-tree 1 () ())
                            (make-tree 7
                                       (make-tree 5 () ())
                                       (make-tree 9
                                                  ()
                                                  (make-tree 11 () ())))))



; レコードの集合のキーが上の木で順序づけられている時の lookup
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (entry set-of-records))
         (entry set-of-records))
        ((< given-key (entry set-of-records))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (entry set-of-records))
         (lookup given-key (right-branch set-of-records)))))


(print (lookup 7 key-tree))
(print (lookup 9 key-tree))
(print (lookup 10 key-tree))
