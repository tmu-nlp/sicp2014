;Pascal三角形の中にn行目mつ目の値を計算する
(define (pascal n m)
  (cond ((< n m) "error")
        ((= m 1) 1)
        ((= n m) 1)
        (else (+ (pascal (- n 1) (- m 1)) 
                 (pascal (- n 1) m)))))
;test
(pascal 5 3)
