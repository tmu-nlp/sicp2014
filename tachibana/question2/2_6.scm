(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc n)
  (+ n 1))

(define (to-s z)
  ((z inc) 0))

(print "(to-s zero)")
(print (to-s zero))

(print "(to-s one)")
(print (to-s one))



