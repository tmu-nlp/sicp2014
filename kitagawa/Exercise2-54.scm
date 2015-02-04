;;; equal は 処理系に実装されているため my-equal? として定義する
(define (my-equal? lst1 lst2)
  (cond ((and (null? lst1) (null? lst2)) #t)
        ((null? lst1) #f)
        ((null? lst2) #f)
        ((eq? (car lst1) (car lst2)) (my-equal? (cdr lst1) (cdr lst2)))
        (else #f)))

(my-equal? '(this is a list) '(this is a list))
(my-equal? '(this is a list) '(this (is a) list))
