;ex1_37.scm 

;(a) recursive
(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20)

;(b) iterative
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20)