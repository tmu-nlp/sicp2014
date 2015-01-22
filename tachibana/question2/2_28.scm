(define (fringe tree)
  (define (iter return rest)
    (if (null? rest)
        return
        (if (list? (car rest))
          (iter (append return (iter (list) (car rest))) (cdr rest))
        (iter (append return (list (car rest))) (cdr rest))))
  )
  (iter (list) tree)
)

(define x (list (list 1 2) (list 3 4)))

(print "(fringe x)")
(print (fringe (list (list 1 2) (list 3 4))))
