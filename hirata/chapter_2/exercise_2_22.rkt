(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (expt (car things) 2)
                    answer))))
  (iter items '()))    

(display (square-list (list 1 2 3 4 5 6)))
(newline)

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things) 
        answer
        (iter (cdr things) 
              (cons answer
                    (expt (car things) 2)))))
  (iter items '()))

(display (square-list2 (list 1 2 3 4 5 6)))