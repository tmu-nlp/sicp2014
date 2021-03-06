(define (same-parity first-number . items)
  (define (iter return-list items first-number-remainder)
  	(if (null? items)
      return-list
  	  (if (=  first-number-remainder (remainder (car items) 2))
  	      (iter (append return-list (list (car items))) (cdr items) first-number-remainder)
  	      (iter return-list (cdr items) first-number-remainder))))
  (let ((first-number-remainder (remainder first-number 2))
  	    (return-list (list first-number)))
  	   (iter return-list items first-number-remainder))
)

(print "(same-parity 1 2 3 4 5 6 7)")
(print (same-parity 1 2 3 4 5 6 7))

(print (same-parity 2 3 4 5 6 7))
