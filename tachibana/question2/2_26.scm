(define (reverse items)
  (append (cdr items) (car items))
)

(define (deep-reverse items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items))))
)

; (print "(div-interval A B)")

