(define (entry tree)
  (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-2 tree)
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

;;; 見づらいので、 let* を使って書きなおす
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts)
                                         right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

(define (union-set-tree set1 set2)
  ;; 問題2.62 で作成した 順序リストによる union-set を再利用
  (define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2)
                    (cons x1 (union-set (cdr set1) (cdr set2))))
                   ((< x1 x2)
                    (cons x1 (union-set (cdr set1) set2)))
                   ((> x1 x2)
                    (cons x2 (union-set set1 (cdr set2)))))))))
  (let ((lst1 (tree->list-2 set1))
        (lst2 (tree->list-2 set2)))
    (list->tree (union-set lst1 lst2))))

(define (intersection-set-tree set1 set2)
  (define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-set (cdr set1)
                                         (cdr set2))))
                ((< x1 x2)
                 (intersection-set (cdr set1) set2))
                ((< x2 x1)
                 (intersection-set set1 (cdr set2)))))))
  (let ((lst1 (tree->list-2 set1))
        (lst2 (tree->list-2 set2)))
    (list->tree (intersection-set lst1 lst2))))


;;; 使用例
(define set1 (list->tree (list 4 5 8)))
(define set2 (list->tree (list 3 8 10)))

(tree->list-2 (union-set-tree set1 set2)) ; (3 4 5 8 10)

(tree->list-2 (intersection-set-tree set1 set2)) ; (8)
