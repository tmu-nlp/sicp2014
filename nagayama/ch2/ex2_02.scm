#lang scheme

(define (make-segment p q)
  (if (and (pair? p) (pair? q))
      (cons p q)
      #f))

(define (start-segment line) (car line))
(define (end-segment line) (cdr line))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment line)
  (define a (start-segment line))
  (define b (end-segment line))
  (cons (/ (+ (x-point a) (x-point b)) 2.0)
        (/ (+ (y-point a) (y-point b)) 2.0)))

(define (print-point p)
  ; (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment line)
  ; (newline)
  (display "(")
  (print-point (start-segment line))
  (display ",")
  (print-point (end-segment line))
  ; (newline)
  (display ")"))


; #| 以下実行テスト
; run
(define a (make-point 1 2))
(define b (make-point -2 -3))
(define c (make-point -3 2))
(define d (make-point 0 -1))

(define line1 (make-segment a b))
(define line2 (make-segment c d))

(define line3
  (make-segment
   (midpoint-segment line1)
   (midpoint-segment line2)))

(display "line1 = ") (print-segment line1) (newline)
(display "line2 = ") (print-segment line2) (newline)
(display "line3 = ") (print-segment line3) (newline)
; |#