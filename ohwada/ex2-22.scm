(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items ()))


(print (square-list (list 1 2 3 4))) ; (16 9 4 1)

; $BM_$7$+$C$?$N$H5U=g$N%j%9%H$K$J$C$F$7$^$&M}M3$O!"(B
; $B7W;;$7$?(B 2 $B>h$NCM$r(B answer $B$NA0$K7R$2$F$$$C$F$$$k$+$i!#(B



(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))


(print (square-list (list 1 2 3 4))) ; ((((() . 1) . 4) . 9) . 16)

; $B$3$l$,$A$c$s$HF0$+$J$$M}M3$O!"(B
; (cons 1 ()) $B$O(B (1) $B$K$J$k$,!"(B(cons () 1) $B$O(B (() . 1) $B$K$J$C$F$7$^$&$?$a!#(B
; $B$3$l$O%j%9%H$NKvHx$N(B nil $B$,$J$$$?$a5/$3$k!#(B
