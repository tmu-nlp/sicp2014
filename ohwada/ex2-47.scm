(define (make-vect x y) (cons x y))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define X (make-frame (make-vect 20 20) (make-vect 50 0) (make-vect 0 50)))

; 一つ目のフレームの constructor に対応する selector
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(print (origin-frame X)) ; (20 . 20)
(print (edge1-frame X)) ; (50 . 0)
(print (edge2-frame X)) ; (0 . 50)


(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define X (make-frame (make-vect 20 20) (make-vect 50 0) (make-vect 0 50)))

; 二つ目の constructor に対応する selector
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))


(print (origin-frame X)) ; (20 . 20)
(print (edge1-frame X)) ; (50 . 0)
(print (edge2-frame X)) ; (0 . 50)
