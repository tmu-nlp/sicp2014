#lang racket
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect vect-1 vect-2)
  (make-vect
   (+ (xcor-vect vect-1) (xcor-vect vect-2))
   (+ (ycor-vect vect-1) (ycor-vect vect-2))))

(define (sub-vect vect-1 vect-2)
  (make-vect
   (- (xcor-vect vect-1) (xcor-vect vect-2))
   (- (ycor-vect vect-1) (ycor-vect vect-2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define make-segment cons)
(define (start-segment s)
  (car s))
(define (end-segment s)
  (add-vect (start-segment s) (cdr s)))

(define (draw-line vect1 vect2)
  (display (car vect1))
  (display ",")
  (display (cdr vect1))
  (display ",")
  (display (car vect2))
  (display ",")
  (display (cdr vect2))
  (display ",")
  (newline)
  )

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame)) 
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ((frame-coord-map frame)
                   (start-segment segment)) 
                  ((frame-coord-map frame)
                   (end-segment segment))))
     segment-list)))

(define draw-frame-outline
  (let ((v0 (make-vect 0.0 0.0))
        (v1 (make-vect 1.0 0.0))
        (v2 (make-vect 1.0 1.0))
        (v3 (make-vect 0.0 1.0)))
    (segments->painter
     (list (make-segment v0 v1)
           (make-segment v1 v2)
           (make-segment v0 v3)
           (make-segment v3 v2)))))

(draw-frame-outline (list (cons 0 0) (cons 0 100) (cons 100 0)))

(define draw-frame-cross
  (let ((v0 (make-vect 0 0))
        (v1 (make-vect 1 0))
        (v2 (make-vect 1 1))
        (v3 (make-vect 0 1)))
       (segments->painter
         (list (make-segment v0 v2)
               (make-segment v1 v3)))))

(draw-frame-cross (list (cons 100 100) (cons 0 100) (cons 100 0)))

(define wave
  (segments->painter
    (list (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
          (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
          (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65))
          (make-segment (make-vect 0.40 0.65) (make-vect 0.30 0.65))
          (make-segment (make-vect 0.75 0.65) (make-vect 1.00 0.35))
          (make-segment (make-vect 0.60 0.45) (make-vect 1.00 0.15))
          (make-segment (make-vect 0.60 0.45) (make-vect 0.75 0.00))
          (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
          (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.60))
          (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40))
          (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85))
          (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65))
          (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.50))
          (make-segment (make-vect 0.35 0.50) (make-vect 0.25 0.00))
          (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00)))))

(wave (list (cons 100 100) (cons 0 100) (cons 100 0)))
