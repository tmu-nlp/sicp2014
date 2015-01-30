; #lang scheme

; 作用準評価と正規順評価の比較


; 作用順評価 : 引数を評価し、作用させてから展開
; 正規順評価 : 展開し、簡約化してから引数を代入

; (define (gcd a b)
;   (if (= b 0)
;       a
;       (gcd b (rem a b)))

(let (count 0)
(define (gcd a b count)
  (if (= b 0)
      (begin a
             count)
      (begin (let count (+ count 1)) 
             (gcd b (car (remainder a b) count)))))
)

;run
(gcd 206 40 0)

;
; 正規順評価では、
; 
; (gcd 206 40)
;   |
; (gcd  40 (remainder 206 40))
;   |
; (gcd  (remainder 206 40)
;       (remainder 40 (remainder 206 40)))
;   |
; (gcd  (remainder 40 (remainder 206 40))
;       (remainder (remainder 206 40)
;                  (remainder 40 (remainder 206 40))))
;   |
;
; 完全に展開することが出来ない。
;
;
; 作用順評価では、
; 
; (gcd 206 40)
;   |
; (gcd  40  (remainder 206 40))
;   |
; (gcd  40  6)
;   |
; (gcd   6  (remainder  40  6)) 
;   |
; (gcd   6  4)
;   |
; (gcd   4  (remainder   6  4))
;   |
; (gcd   4  2)
;   |
; (gcd   2  (remainder   4  2))
;   |
; (gcd   2  0)
;   |
;   2
;   
; remainder は 4 回適用された。
;  
