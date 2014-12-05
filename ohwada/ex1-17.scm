(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;------------------------------------
; 対数的ステップ数の乗算プロセス

(define (double a) (* a 2))
(define (halve a) (/ a 2))

(define (even? n)
  (= (remainder n 2) 0))


(define (fast-* b n)
  (cond ((= n 0) 0)
        ((= n 1) b)
        ((even? n) (fast-* (double b) (halve n)))
        (else (+ b (fast-* b (- n 1))))))


(print (fast-* 2 0))   ; 0
(print (fast-* 2 1))   ; 2
(print (fast-* 2 2))   ; 4
(print (fast-* 2 3))   ; 6
(print (fast-* 2 9))   ; 18
(print (fast-* 3 13))  ; 39      
