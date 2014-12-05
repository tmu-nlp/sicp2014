;ex1_39.scm
;D = [1,3,5,...]
;N = [x,-x^2,-x^2,..]

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* i 2.0) 1))
             k))
(tan-cf 10 100)