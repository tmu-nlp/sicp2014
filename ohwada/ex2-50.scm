; $B%U%l!<%`$N(B constructor $B$H(B selector
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))


(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))



; $B%Y%/%H%k$N(B constructor $B$H(B selector
(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))


(define (add-vect a b)
  (make-vect
   (+ (xcor-vect a) (xcor-vect b)) (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b)) (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s a)
  (make-vect
   (* s (xcor-vect a)) (* s (ycor-vect a))))



; frame-coord-map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))



; segment $B$N(Bconstructor $B$H(B selector
(define (make-segment vect1 vect2) (cons vect1 vect2))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))



; draw-line (2$BE@4V$K@~$r0z$/(B)
(define (draw-line v1 v2)
  (display (xcor-vect v1))
  (display ",")
  (display (ycor-vect v1))
  (display ",")
  (display (xcor-vect v2))
  (display ",")
  (display (ycor-vect v2))
  (newline))




; segments->painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))




; $B%U%l!<%`(BX $B$rDj5A(B
(define X (make-frame (make-vect 150 150) (make-vect 100 0) (make-vect 0 100)))



; $B;XDj$5$l$?%U%l!<%`$N307A$rIA$/%Z%$%s%?(B
(define outline-frame
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0)))))


(print (outline-frame X))





; transform-painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


; shrink-to-upper-right
(define (shrink-to-upper-right painter)
  (transform-painter painter
                      (make-vect 0.5 0.5)
                      (make-vect 1.0 0.5)
                      (make-vect 0.5 1.0)))

;(print ((shrink-to-upper-right outline-frame) X))




; flip-horiz
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(print ((flip-horiz outline-frame) X))


; $BH?;~7W<~$j$K(B 180$BEY2sE>(B
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))


(print ((rotate180 outline-frame) X))


; 270$BEY2sE>(B
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


(print ((rotate270 outline-frame) X))
