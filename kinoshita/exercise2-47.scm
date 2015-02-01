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

(define f (make-frame (make-vect 1 1) (make-vect 3 4) (make-vect 0 2)))
(print (origin-frame f))
(print (edge1-frame f))
(print (edge2-frame f))

(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (cddr frame))

(define f (make-frame (make-vect 1 1) (make-vect 3 4) (make-vect 0 2)))
(print (origin-frame f))
(print (edge1-frame f))
(print (edge2-frame f))
