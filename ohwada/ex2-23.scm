; for-each

(define (for-each act items)
  (cond ((null? items) #t)
        (else (act (car items)) (for-each act (cdr items)))))


(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))
