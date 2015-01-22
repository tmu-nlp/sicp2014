(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(define (tree-map square tree)
	(cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons (tree-map square (car tree))
                    (tree-map square (cdr tree))))))
