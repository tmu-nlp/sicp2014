#lang scheme

; Louis の square-list 手続きの添削

(define (square x) (* x x))

; Louis 1
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items (list)))

 ; cons によるのリストの接続が逆なため失敗した
 ; answer の変遷
 ; ()
 ; (1 ())
 ; (2 (1 ()))
 ; (3 (2 (1 ())))
 ; ...

; Louis 2
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))

 ; answer の入れ子構造がリストの形式になっていない
 ; 実際の結果 -> ((((() . 1) . 4) . 9) . 16)
 ; 欲しかった -> (1 . (4 . (9 . (16 . ()))))


; run
(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))