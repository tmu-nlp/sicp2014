(use srfi-27)

(define (square x)
  (* x x))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define pair (cons 2 3)) 
(print (car pair)) 
(print (cdr pair)) 

(print zero)
(print add-1)
;進捗ダメです
