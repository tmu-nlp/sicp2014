#lang scheme

; 長方形の面積や周長を求める表現
; 抽象化バリアを用いてどんな表現をしても求められるように

; 四角 -> cons 辺
; 辺　 -> cons 点
; 点　 -> cons 数

; 前提関数
(define (square x) (* x x))

; 点 = (x, y)
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; 辺 = (始点, 終点)
(define (make-segment p q)
  (if (and (pair? p) (pair? q))
      (cons p q)
      #f))

; 辺の始点と終点
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))

; 平行四辺形 = (縦辺, 横辺)
(define (make-rec e1 e2)
  (if (and (pair? e1) (pair? e2))
      (cons e1 e2)
      (if (and (not (pair? e1)) (not (pair? e2)))
          (cons e1 e2)
          #f)))

; 辺の長さを返す
(define (length segment)
  (if (pair? segment)
      (let ((p (car segment))
            (q (cdr segment)))
        (if (and (pair? p) (pair? q))
            (sqrt (+ (square (- (car q) (car p)))
                     (square (- (cdr q) (cdr p)))))
            (if (and (not (pair? p)) (not (pair? q)))
                (sqrt (+ (square p) (square q)))
                (error "error length"))))
      (abs segment)))

; 長方形の面積
(define (rec_space rectangle) 
  (if (pair? rectangle)
      (let ((l1 (car rectangle))
            (l2 (cdr rectangle)))
        (if (vert? l1 l2)
            (* (length l1) (length l2))
            #f))
      #f))

; 長方形の周長
(define (surround_edge rectangle)
  (if (pair? rectangle)
      (let ((l1 (car rectangle))
            (l2 (cdr rectangle)))
        (if (vert? l1 l2)
            (* 2 (+ (length l1) (length l2)))
            #f))
      #f))

; 直交検査
(define (vert? segment1 segment2)
  (if (and (pair? segment1) (pair? segment2))
      (let ((p1 (car segment1))
            (p2 (cdr segment1))
            (q1 (car segment2))
            (q2 (cdr segment2)))
        (if (and (pair? p1) (pair? p2) (pair? q1) (pair? q2))
            (if (= (+ (* (- (car p2) (car p1))
                         (- (car q2) (car q1)))
                      (* (- (cdr p2) (cdr p1))
                         (- (cdr q2) (cdr q1))))
                   0)
                #t    ; 直交な2点指定線分
                #f)
            #f))
      (if (and (not (pair? segment1)) (not (pair? segment2)))
          #t    ; スカラー
          #f)))
 

; #| 表示文
(define (print-segment line)
  ; (newline)
  (display "(")
  (print-point (start-segment line))
  (display ",")
  (print-point (end-segment line))
  ; (newline)
  (display ")"))

(define (print-point p)
  ; (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
; |#

; 以下、実行テスト

; 頂点
(define a (make-point 0 0))
(define b (make-point 3 0))
(define c (make-point 0 0))
(define d (make-point 0 3))

; 縦辺, 横辺
(define line1 (make-segment a b))
(define line2 (make-segment c d))

; 四角形
(define quad (make-rec line1 line2))
(define quad_2 (make-rec 4 3))

; 面積, 周長
(rec_space quad)
(surround_edge quad)
(rec_space quad_2)
(surround_edge quad_2)