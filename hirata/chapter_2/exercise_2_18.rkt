(define (append list1 list2)
  (if (null? list1)
   list2
   (cons (car list1)
         (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
      items
  (append (reverse (cdr items)) (list (cdr items)))))

;(define odds (list 1 2 3 4))
(reverse (list 1 2 3 4))
;(display (reverse odds))