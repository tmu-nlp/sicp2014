(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

;O(1)
(define (adjoin-set x set)
    (cons x set))

;O(n^2)
(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
            (cons (car set1)
                  (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

;O(n)
(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (adjoin-set (car set1)
                            (union-set (cdr set1) set2)))))
;O(1)
(define (union-set set1 set2)
    (append set1 set2))

(define set1 (list 1 2 3 4))
(define set2 (list 3 4 5 6))

(print (intersection-set set1 set2))
(print (union-set set1 set2))
