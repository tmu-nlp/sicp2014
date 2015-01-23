;最初のプログラム
(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))
;(1^2 . ())→(2^2 . (1^2 . ()))→(3^2 . (2^2 . (1^2 . ())))というように評価されていってしまうから、結果が逆順になってしまう

;修正後
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
;(() . 1^2)→((() . 1^2) . 2^2)→というようにconsしていくため、うまくいかない


#?=(square-list-1 (list 1 2 3 4))
#?=(square-list-2 (list 1 2 3 4))
