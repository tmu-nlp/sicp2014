(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-rectangle start end)
  (cons start end))

;長方形の横幅
(define (width rectangle)
    (let* ((p1 (start-segment rectangle))
         (p2 (end-segment rectangle))
         (x1 (x-point p1))
         (x2 (x-point p2)))
      (abs (- x1 x2))))

;縦の幅
(define (hight rectangle)
    (let* ((p1 (start-segment rectangle))
         (p2 (end-segment rectangle))
         (y1 (y-point p1))
         (y2 (y-point p2)))
      (abs (- y1 y2))))

;周囲の長さ
(define (araund rectangle)
  (+ (width rectangle) (hight rectangle)))

;面積
(define (area rectangle)
  (* (width rectangle) (hight rectangle)))

(define a (make-point 1 2))
(define b (make-point -5 -12))
(define c (make-rectangle a b))
(define d (area c))
d



