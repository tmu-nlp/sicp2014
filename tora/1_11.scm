;f(n)<3のときf(n)=n、f(n)>=3のときf(n)=f(n-1)+2f(n-2)+3f(n-3)
;
;再帰的プロセス
;(define (f n)
;  (cond ((= n 0) 0)
;        ((= n 1) 1)
;        ((= n 2) 2)
;        ((> n 2)
;         (+ (f (- n 1))
;            (* 2 (f (- n 2)))
;            (* 3 (f (- n 3)))
;            ))))

;反復的プロセス
(define (f n) (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (if (= count 0) 
      c
      (f-iter (+ a (* 2 b) (* 3 c))
              a 
              b 
              (- count 1))))

;test:
(f 10)

