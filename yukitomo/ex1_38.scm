;ex1_38.scm
(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

;N = [1,1,1,....] 
;D = [1,2,1,1,4,1,1,6,1,1,8,....]

(define (D k)
  (if (= (remainder (+ k 1) 3) 0)
      (* 2 (/ (+ k 1) 3))
      1))

(+ 2 (cont-frac (lambda (x) 1.0) D 50))
