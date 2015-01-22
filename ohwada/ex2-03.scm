; make-segment と make-point

(define (make-segment a b)
  (cons a b))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))


(define (make-point x y)
  (cons x y))

(define (x-point x) (car x))
(define (y-point x) (cdr x))


; make-rectangle ( 構成子 ) : 線分( 始点 (x1, y1), 終点 (x2, y2) )
;                             と高さが与えられるとする

(define (make-rectangle segment height)
  (cons segment height))

; segment と height ( 選択子 ) → 線分と高さを返す

(define (segment rectangle) (car rectangle))
  
(define (height rectangle) (cdr rectangle))


; cul-perimeter と cul-area

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

; (1, 0) と (5, 0) の距離 → 4
; perimeter → 4 * 2 + 6 * 2 = 20
; area → 4 * 6 = 24
