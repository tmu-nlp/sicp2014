(define x (list (list 1 2) (list 3 4)))

(define (fringe items)
  (cond ((null? items) items)
        ((pair? items) (append (fringe (car items)) (fringe (cdr items))))
        (else (list items)))) 


(print (fringe x)) ; (1 2 3 4)
(print (fringe (list x x))) ; (1 2 3 4 1 2 3 4)
