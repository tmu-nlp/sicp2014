;(define (make-vect x y)
;  (cons x y)) 

;(define (xcor-vect v)
;  (car v)) 

;(define (ycor-vect v)
;  (cdr v)) 


(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))


;; example
(define v1 (make-vect 1 2))
(define v2 (make-vect 3 5))
(define s (make-segment v1 v2))

(display (start-segment s))              ; (1 . 2)
(display (end-segment s))                ; (3 . 5)
