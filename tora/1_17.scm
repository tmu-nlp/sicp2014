;再帰的プロセス
;乗算
(define (times a b)
  (if (= b 0)
    0
    (+ a (times a (- b 1)))))
;test
;(times 3 4)

;二倍算
;アイディア：2n = 2(n-1)+2
(define (double n)
  (if (= n 1) 
      2
      (+ 2 (double (- n 1)))))
;test
;(double 3)

;二分算
;アイディア：n/2 = (n-2)/2+1
(define (halve n)
  (if (= n 2)
      1
      (+ 1 (halve (- n 2)))))
;test
;(halve 6)
      
