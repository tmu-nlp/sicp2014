;ex2_5.scm

;x^y
(define (pow x y)
    (if (= y 0)
        1
        (* x (pow x (- y 1)))))

;cがdで何回割れるか
(define (dev-count c d)
  (define (dev x y count)
    (if (= (remainder x y) 0)
        (dev (/ x y) y (+ count 1))
        count))
  (dev c d 0))

(define (cons a b) 
  (* (pow 2 a) (pow 3 b)))

(define (car c)
  (dev-count c 2))
  
(define (cdr c)
  (dev-count c 3))

(car (cons 3 5))
(cdr (cons 3 5))
