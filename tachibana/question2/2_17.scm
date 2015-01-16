(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(print "(last-pair (list 23 72 149 34))")
(print (last-pair (list 23 72 149 34)))