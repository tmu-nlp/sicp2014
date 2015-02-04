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


; adjoin-set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)    ; ← このイコールを入れないと重複が発生してしまう
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(print (adjoin-set 7 (list 3 7 11))) ; (3 7 11)
(print (adjoin-set 6 (list 3 7 11))) ; (3 6 7 11)
(print (adjoin-set 14 (list 3 7 11))) ; (3 7 11 14)
