#lang scheme

; 指定されたリストから 7 を取り出す
; car,cdr の組み合わせを考える

; [1]  (1 3 (5 7) 9)
(list 1 3 (list 5 7) 9)
(car (cdr (car (cdr (cdr 
                (list 1 3 (list 5 7) 9)
                ))))) 

; [2]  ((7))
(list (list 7))
(car (car 
     (list (list 7))
     ))


; [3]  (1 (2 (3 (4 (5 (6 7))))))
(list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))
; (cadadadadadadr
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))
 ))))))))))))
 