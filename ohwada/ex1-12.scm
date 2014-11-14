; a は何段目か
; b は左から何番目か

(define (pas-tri a b)
  (cond ((or (= b 1) (= a b)) 1)
        (else (+ (pas-tri (- a 1) (- b 1))
                 (pas-tri (- a 1) b)))))

(print (pas-tri 5 3))   ; 6
(print (pas-tri 6 4))   ; 10
(print (pas-tri 7 5))   ; 15
