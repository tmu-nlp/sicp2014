; last-pair

(define (last-pair items)
  (list-ref items (- (length items) 1)))


(print (list (last-pair (list 23 72 149 34))))
