; 順序ありのリストとしての集合

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))



; union-set
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set  set1 (cdr set2))))
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))))



(print (union-set (list 1 4 6 9 11) (list 3 4 6 8 10 13))) ; (1 3 4 6 8 9 10 11 13)

