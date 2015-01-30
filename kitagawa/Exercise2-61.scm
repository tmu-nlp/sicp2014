(define (adjoin-set x set)
  (if (null? set)
      (cons x '())
      (let ((x1 (car set)))
        (cond ((= x x1) set)
              ((> x x1)
               (cons x1 (adjoin-set x (cdr set))))
              ((< x x1)
               (cons x set))))))


;;; 使用例
(define set (list 1 3 5 10))

(adjoin-set 5 set)                      ; (1 3 5 10)
(adjoin-set 2 set)                      ; (1 2 3 5 10)
(adjoin-set 7 set)                      ; (1 3 5 7 10)
(adjoin-set 8 '())                      ; (8)
