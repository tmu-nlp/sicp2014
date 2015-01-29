(define (square x) (* x x))
(define X (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(print X) ; (1 (2 (3 4) 5) (6 7))

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(print (square-tree X)) ; (1 (4 (9 16) 25) (36 49))


(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(print (square-tree X)) ; (1 (4 (9 16) 25) (36 49))


