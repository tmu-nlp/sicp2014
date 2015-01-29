(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(print (fold-right / 1 (list 1 2 3)))
(print (fold-left / 1 (list 1 2 3)))
(print (fold-right list () (list 1 2 3)))
(print (fold-left list () (list 1 2 3)))

;結合則を満たす演算は結果が等しくなる
;(a*b)*c = a*(b*c)
