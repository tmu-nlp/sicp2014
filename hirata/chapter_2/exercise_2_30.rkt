(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (expt tree 2))
        (else
          (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
; (1 (2 (3 4) 5) (6 7))

(display (square-tree x))