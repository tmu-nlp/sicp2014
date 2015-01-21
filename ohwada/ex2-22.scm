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

; 欲しかったのと逆順のリストになってしまう理由は、
; 計算した 2 乗の値を answer の前に繋げていっているから。



(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))


(print (square-list (list 1 2 3 4))) ; ((((() . 1) . 4) . 9) . 16)

; これがちゃんと動かない理由は、
; (cons 1 ()) は (1) になるが、(cons () 1) は (() . 1) になってしまうため。
; これはリストの末尾の nil がないため起こる。
