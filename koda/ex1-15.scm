(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(print (sine 12.15))

;;a) 12.15,4.05,1.35,0.45 と続くため3回
;;b) どちらも,O(log3(a))

