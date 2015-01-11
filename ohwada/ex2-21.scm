; 2 通りの square-list

(define (square-list items)
  (if (null? items)
      ()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(print (square-list (list 1 2 3 4)))



(define (square-list items)
  (map (lambda (x) (* x x))
       items))

(print (square-list (list 1 2 3 4)))
