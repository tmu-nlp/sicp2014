(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else
          (cons (tree-map proc (car tree))
                (tree-map proc (cdr tree))))))

(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(display x)
(newline)

(define (square x) (* x x))

(define (square-tree tree)
  (tree-map square tree))

(display (square-tree x))