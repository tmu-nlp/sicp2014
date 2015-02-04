(define (make-vect x y)
    (cons x y))

(define (xcor-vect v)
    (car v))

(define (ycor-vect v)
    (cdr v))

(define (add-vect v w)
    (cons (+ (xcor-vect v) (xcor-vect w))
          (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
    (cons (- (xcor-vect v) (xcor-vect w))
          (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect v s)
    (cons (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (caddr frame))

(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (cddr frame))

(define (make-segment start-vect end-vect)
    (cons start-vect end-vect))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define s (make-segment (make-vect 1 1) (make-vect 2 2)))
(print (start-segment s))
(print (end-segment s))
