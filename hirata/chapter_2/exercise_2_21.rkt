(define (square-list items) 
  (if (null? items)
      '()
      (cons (expt (car items) 2) 
            (square-list (cdr items)))))
(define (square-list items)
(map (lambda (item) (expt item 2)) items))

(display (square-list (list 1 2 3 4 5 6)))