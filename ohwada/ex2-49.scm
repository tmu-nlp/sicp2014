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



; segment $B$N(B constructor $B$H(B selector
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
(define X (make-frame (make-vect 200 200) (make-vect 100 0) (make-vect 0 100)))


; a. $B;XDj$5$l$?%U%l!<%`$N307A$rIA$/%Z%$%s%?(B
(define outline-frame
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0)))))


(print (outline-frame X))



; b. $B%U%l!<%`$N8~$+$$9g$&D:E@F1;N$r7k$s$G(B"X"$B$rIA$/%Z%$%s%?(B
(define draw-x
  (segments->painter
    (list
     (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
     (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))


(print (draw-x X))


; c. $B%U%l!<%`$N3FJU$NCfE@$r7k$s$GI)7A$rIA$/%Z%$%s%?(B
(define draw-rhombus
  (segments->painter
    (let ((mid1 (scale-vect 0.5
                  (add-vect (make-vect 0.0 0.0) (make-vect 1.0 0.0))))
          (mid2 (scale-vect 0.5
                  (add-vect (make-vect 0.0 0.0) (make-vect 0.0 1.0))))
          (mid3 (scale-vect 0.5
                  (add-vect (make-vect 1.0 0.0) (make-vect 1.0 1.0))))
          (mid4 (scale-vect 0.5
                  (add-vect (make-vect 0.0 1.0) (make-vect 1.0 1.0)))))
      (list
       (make-segment mid1 mid2)
       (make-segment mid1 mid3)
       (make-segment mid2 mid4)
       (make-segment mid3 mid4)))))


(print (draw-rhombus X))


; d. wave $B%Z%$%s%?(B
(define wave
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))

(print (wave X))
