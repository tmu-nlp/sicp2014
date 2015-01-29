(define (square x) (* x x))

(define (tree-map func tree)
    (cond ((null? tree) ())
          ((not (pair? tree)) (func tree))
          (else (cons (tree-map func (car tree))
                      (tree-map func (cdr tree))))))

(define t
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
(define (square-tree tree)
    (tree-map square tree))

(print (square-tree t))

(define (tree-map func tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map func sub-tree)
                (func sub-tree)))
            tree))

(print (square-tree t))
