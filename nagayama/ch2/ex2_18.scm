#lang scheme

; reverse : 逆順のリストを返す

(define (reverse s)
  (define (iter s t)
    (if (null? s)
        t
        (iter (cdr s) (cons (car s) t))))
  (if (list? s)
      (iter s (list))
      #f))

; run
(reverse (list 1 4 9 16 25))
