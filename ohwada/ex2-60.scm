(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) (append set1 set2))



(print (union-set (list 3 6 4 1 5) (list 4 9 2 1))) ; (3 6 4 1 5 4 9 2 1)
(print (intersection-set (list 1 3 3 3 2) (list 3 3))) ; (3 3)

; adjoin $B$H(B union $B$,8zN(E*$K=hM}$G$-$k$h$&$K$J$k(B(adjoin $B$O&((B(1), union $B$O&((B(n))
; union $B$N(B append $B$O&((B(n)$B$@$,!"(B2.59 $B$N(B union $B$O(B element-of-set? ($B&((B(n)) $B$r(B set1 $B$N(B 
; $BMWAG?t2s8F$S=P$9$N$G%9%F%C%W?t$O(B $B&((B(n^2)$B!#=>$C$F$3$A$i$NJ}$,B.$$(B

;$BMWAG?t$,A}$($k$N$G(B element-of-set? $B$OCY$/$J$k(B $B"*(B intersection $B$O99$KCY$/$J$k(B
 
