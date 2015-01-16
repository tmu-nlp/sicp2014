(define (for-each proc items)
  (if (null? items)
      (newline)
      ((proc (car items)) (for-each proc (cdr items))) ;違う作業を行う表現がよくわからない
  )
)

; (define (for-each f s)
;   (cond ((null? s) (newline))
;         (else (f (car s))
;               (for-each f (cdr s))
;         )
;   )
; )

; (print "(div-interval A B)")


