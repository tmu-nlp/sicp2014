;反復的プロセス
;乗算
(define (times a b)
  (times-iter a b 0))
(define (times-iter a b product)
  (if (= b 0)
      product
      (times-iter a  (- b 1) (+ product a))))
;test
;(times 3 4)

;二倍算
;アイディア：2n=(n-1)+(n+1)=((n-1)-1)+((n-1)+1)=...=0+2n
(define (double n)
  (double-iter n n))
(define (double-iter n product)
  (if (= n 0) 
      product
      (double-iter (- n 1) (+ product 1))))
;test
;(double 3)

;;二分算
;;アイディア：n/2 = (0+n)/2 = (1+(n-1)/2) = ... = ((n/2)+(n/2))/2
(define (halve n)
  (halve-iter n 0))
(define (halve-iter n product)
  (if (= n product)
      product
      (halve-iter (- n 1) (+ product 1))))
;test
(halve 6)
      
