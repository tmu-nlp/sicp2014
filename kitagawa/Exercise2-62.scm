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


;;; 使用例
(define set1 (list 1 3 5 10))
(define set2 (list 2 4 5 13))

(union-set set1 set2)                   ; (1 2 3 4 5 10 13)
(union-set set2 set1)                   ; (1 2 3 4 5 10 13)
(union-set set1 '())                    ; (1 3 5 10)
(union-set '() '())                     ; ()

