;ex2_3.scm
(define (square x) (* x x))
(define (avg a b) (/ (+ a b) 2))

;点、線分の定義
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (cons
   (avg (x-point (start-segment seg))
        (x-point (end-segment seg)))
   (avg (y-point (start-segment seg))
        (y-point (end-segment seg)))))

;線分の長さの定義
(define (length-segment seg)
  (sqrt (+ (square (- (x-point (start-segment seg))
                      (x-point (end-segment seg))))
           (square (- (y-point (start-segment seg))
                      (y-point (end-segment seg)))))))



;長方形の定義(線分2本で構成)
(define (make-rectangle seg-x seg-b) (cons seg-x seg-y))
(define (height-rectangle rec) (length-segment (car rec)))
(define (width-rectangle rec) (length-segment (cdr rec)))

;perimeter , space
(define (perimeter r)
  (+ (* 2 (height-rectangle r))
     (* 2 (width-rectangle r))))
(define (area r)
  (* (height-rectangle r) (width-rectangle r)))

;test
(define seg-x (make-segment (make-point 0 0) (make-point 4 0)))
(define seg-y (make-segment (make-point 0 0) (make-point 0 6)))
  
(define rec1 (make-rectangle seg-x seg-y))
(perimeter rec1)
(area rec1)

;横、縦の長さで定義するもの
(define (make-rectangle height width) (cons height width))
(define (height-rectangle a) (car a))
(define (width-rectangle a) (cdr a))

(define rec2 (make-rectangle 4 6))
(perimeter rec2)
(area rec2)