(define (entry tree)
  (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

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

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((record (entry set-of-records)))
        (cond ((equal? given-key (key record)) record)
              ((< given-key (key record))
               (lookup given-key (left-branch set-of-records)))
              ((> given-key (key record))
               (lookup given-key (right-branch set-of-records)))))))

(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

;;; 使用例
(define records
  (list->tree (list (make-record 1 "hoge")
                    (make-record 2 "foo")
                    (make-record 3 "bar"))))

;;; 使用例
(lookup 1 records)                      ; (1 . "hoge")
(lookup 2 records)                      ; (2 . "foo")
(lookup 4 records)                      ; #f
