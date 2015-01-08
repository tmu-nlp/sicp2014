#lang racket

"バグver"
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (sqr (car things))
                    answer))))
  (iter items null))

(square-list (list 1 2 3 4))
'(1 4 9 16)

;; 理由
;; (1^2 . ())
;; => (2^2 . (1^2 . ()))
;; => (3^2 . (2^2 . (1^2 .())))
;; のように評価され、リストが作られていってしまうから


;; consがリストを返すのは第1引数が値で、
;; 第2引数がリストの場合のみなので、
;; 値をリスト化してappendする
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer 
                      (list (sqr (car things)))))))
  (iter items null))
"修正xer"
(square-list2 (list 1 2 3 4))
'(1 4 9 16)