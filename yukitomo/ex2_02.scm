;ex2_2.scm
(define (avg a b) (/ (+ a b) 2))

;点の定義
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

;線分の定義
(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;線分の中間点を返す手続き
(define (midpoint-segment seg)
  (cons
   (avg (x-point (start-segment seg))
        (x-point (end-segment seg)))
   (avg (y-point (start-segment seg))
        (y-point (end-segment seg)))))

;点を印字する手続き
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;test
;各点の定義
(define point1 (make-point 1 1))
(print-point point1)
(define point2 (make-point 3 3))
(print-point point2)
;線分の定義
(define segment (make-segment point1 point2))

;中間点の出力
(print-point (midpoint-segment segment))

