(define (square-list items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items)) (square-list (cdr items)))
  )
)

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))
    )
  )
  (iter items (list))
 )

;からのリストに対して各要素をconsさせようとしているから


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))
    )
  )
  (iter items (list))
)
;(cons answer (square (car things)) )にしているため
