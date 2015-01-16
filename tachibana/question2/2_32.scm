(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(print (subsets (list 1 2 3)))
(print (subsets (list 2 3)))
(print (subsets (list 3)))
(print (subsets (list)))

;restが(list)になるまでsubsetsは再帰し続け、その(list)と(car s)の対がリスト構造において
;次の上位の層においての(cdr s)となり、それと(car s)の対を繰り返し作るため。