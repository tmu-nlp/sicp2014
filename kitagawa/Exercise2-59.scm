(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))


;;; 使用例
(define set1 (list 1 3 5 10))
(define set2 (list 5 7 2 4))

(union-set set1 set2)                   ; (10 3 1 5 7 2 4)
(union-set set1 '())                    ; (1 3 5 10)
