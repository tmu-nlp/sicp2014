(define A (list 1 3 (list 5 7) 9))
(define B (list (list 7)))
(define C (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))


(print (car (cdr (car (cdr (cdr A))))))
(print (car (car B)))
(print (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr C)))))))))))))
