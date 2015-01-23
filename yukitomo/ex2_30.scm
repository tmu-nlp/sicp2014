;ex2_30.scm

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree tree)
  (map (lambda (item)
         (if (pair? item)
             (square-tree item)
             (* item item)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))