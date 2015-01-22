(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items))))
)

(define (square-list items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items)) (square-list (cdr items)))
  )
)

(define (square-list items)
  (map (lambda (x) (* x x)) items)
)

; (print "(div-interval A B)")

