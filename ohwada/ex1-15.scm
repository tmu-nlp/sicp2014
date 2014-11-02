(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
           angle
           (p (sine (/ angle 3.0)))))


(print (sine 12.15))

; (p (sine 4.05))
; (p ((p (sine 1.35))))
; (p ((p ((p (sine 0.45))))))
; (p ((p ((p ((p (sine 0.15))))))))
;               ((p (sine 0.05)))

; a. 5 回

; b. スペース    θ(log(a))
;    ステップ数  θ(log(a))  


 

