(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) () sequence))

(define x (list 1 2 3 4 5))
(print (reverse x))

(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) () sequence))

(print (reverse x))
