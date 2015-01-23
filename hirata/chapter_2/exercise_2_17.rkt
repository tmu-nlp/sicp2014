(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(define odds (list 1 2 3 4))
(display (last-pair odds))