(define x (list 1 3 (list 5 7) 9)) ; (1 3 (5 7) 9)
(display (car (cdr (car (cdr (cdr x))))))
(newline)

(define y (list (list 7))) ;((7))
(display (car (car y)))
(newline)

(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))) ; (1 (2 (3 (4 (5 (6 7))))))
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x)))))))))))))