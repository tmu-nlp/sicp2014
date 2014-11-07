;ex1_11.scm 
; n < 3, f(n) = n
; n >= 3, f(n) = f(n-1) + 2 f(n-2) + 3 f(n-3)

;recursive
(define (f_r n)
  (cond ((< n 3) n)
        (else (+ (f_r (- n 1)) (* 2 (f_r (- n 2))) (* 3 (f_r (- n 3)))))))
        
;iteration
(define (f_i n)
  (define (f_iteration a b c count)
    (if (= count 0) a
        (f_iteration (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (if (< n 3) n
      (f_iteration 2 1 0 (- n 2))))