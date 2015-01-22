(use srfi-27)

(define (square x)
  (* x x))
(define (inc x)
  (+ x 1))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(print ((zero inc) 0))
(print ((one inc) 0))
(print ((two inc) 0))
;進捗ダメです
