(define (square x) (* x x))

(define (square-tree tree)
    (cond ((null? tree) ())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))

(define t
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(print (car t))
(print (square-tree t))

(define (square-tree tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (square sub-tree)))
            tree))

(print (square-tree t))
