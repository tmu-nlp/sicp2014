; make-segment $B$H(B make-point

(define (make-segment a b)
  (cons a b))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))


(define (make-point x y)
  (cons x y))

(define (x-point x) (car x))
(define (y-point x) (cdr x))


; make-rectangle ( $B9=@.;R(B ) : $B@~J,(B( $B;OE@(B (x1, y1), $B=*E@(B (x2, y2) )
;                             $B$H9b$5$,M?$($i$l$k$H$9$k(B

(define (make-rectangle segment height)
  (cons segment height))

; segment $B$H(B height ( $BA*Br;R(B ) $B"*(B $B@~J,$H9b$5$rJV$9(B

(define (segment rectangle) (car rectangle))
  
(define (height rectangle) (cdr rectangle))


; cul-perimeter $B$H(B cul-area

(define (square x) (* x x))

(define (cul-perimeter rectangle)
  (let ((segment (car rectangle))
        (height (cdr rectangle)))
  (let ((p_a (start-segment segment))
        (p_b (end-segment segment)))
  (let ((x1 (x-point p_a))
        (y1 (y-point p_a))
        (x2 (x-point p_b))
        (y2 (y-point p_b)))
  (+ (* (sqrt (+ (square (- x2 x1)) (square (- y2 y1)))) 2)
     (* height 2))))))


(define (cul-area rectangle)
  (let ((segment (car rectangle))
        (height (cdr rectangle)))
  (let ((p_a (start-segment segment))
        (p_b (end-segment segment)))
  (let ((x1 (x-point p_a))
        (y1 (y-point p_a))
        (x2 (x-point p_b))
        (y2 (y-point p_b)))
  (* (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))
     height)))))


(print (cul-perimeter
         (make-rectangle (make-segment (make-point 1 0) (make-point 5 0))
                         6)))

(print (cul-area
         (make-rectangle (make-segment (make-point 1 0) (make-point 5 0))
                         6)))

; (1, 0) $B$H(B (5, 0) $B$N5wN%(B $B"*(B 4
; perimeter $B"*(B 4 * 2 + 6 * 2 = 20
; area $B"*(B 4 * 6 = 24
