;ex2_17.scm
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;与えられた(空でない)リストの最後の要素だけからなるリストを返す手続き
(define (last-pair items)
  (list-ref items (- (length items) 1)))

(last-pair (list 23 72 149 34))