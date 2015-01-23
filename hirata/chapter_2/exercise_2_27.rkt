(define (deep-reverse lis)
  (define (iter li1 li2)
    (if (null? li1)
        li2
        (let* ((head (car li1))
               (tail (cdr li1))
               (head-rev (if (list? head) (deep-reverse head) head)))
          (iter tail (cons head-rev li2)))))
  (iter lis '()))


(define x (list (list 1 2) (list 3 4)))
(display x)
(newline)
(display (deep-reverse x))