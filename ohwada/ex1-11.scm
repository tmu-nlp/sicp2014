; 再帰的プロセス
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(print (f 2))   ; 2
(print (f 3))   ; 4
(print (f 4))   ; 11
(print (f 10))  ; 1892
(print (f 30))  ; 61354575194 （時間がかかる）

; 反復的プロセス
(define (f n)
  (f-iter 1 2 3 n 0))

(define (f-iter a b c n count)
  (cond ((and (= count 0) (< n 3)) n)
        ((or (and (= count 0) (= n 3)) (and (> count 0) (< (- n 1) 3)))
             (+ (* 2 a) (* 1 b) (* 0 c)))
        (else (f-iter (+ a b) (+ (* a 2) c) (* a 3) (- n 1) (+ count 1)))))   

(print (f 2))   ; 2
(print (f 3))   ; 4
(print (f 4))   ; 11
(print (f 10))  ; 1892
(print (f 30))  ; 61354575194 （すぐ出る）
