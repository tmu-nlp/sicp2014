;;; Vect
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))


;;; Frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;;; 描画用フレーム
;;; 参考: http://www.serendip.ws/archives/816
(define (draw-line v1 v2)
  (display (xcor-vect v1))
  (display ",")
  (display (ycor-vect v1))
  (display ",")
  (display (xcor-vect v2))
  (display ",")
  (display (ycor-vect v2))
  (newline))

(define canvas-frame
  (make-frame (make-vect 0.0 0.0)
              (make-vect 400.0 0.0)
              (make-vect 0.0 400.0)))


;;; Segment
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))


;;; Painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;;; フレームの辺の中点を結んで菱形を描くペインタ
(define diamond->painter
  (let* ((v1 (make-vect 0.5 0.0))
         (v2 (make-vect 0.0 0.5))
         (v3 (make-vect 1.0 0.5))
         (v4 (make-vect 0.5 1.0)))
    (segments->painter
     (list (make-segment v1 v2)
           (make-segment v1 v3)
           (make-segment v2 v4)
           (make-segment v3 v4)))))

;;; wave ペインタ
(define wave
  (segments->painter
   (list (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
         (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
         (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65))
         (make-segment (make-vect 0.40 0.65) (make-vect 0.30 0.65))
         (make-segment (make-vect 0.75 0.65) (make-vect 1.00 0.35))
         (make-segment (make-vect 0.60 0.45) (make-vect 1.00 0.15))
         (make-segment (make-vect 0.60 0.45) (make-vect 0.75 0.00))
         (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
         (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.60))
         (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40))
         (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85))
         (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65))
         (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.50))
         (make-segment (make-vect 0.35 0.50) (make-vect 0.25 0.00))
         (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((painter-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (painter-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (painter-left frame)
        (painter-right frame)))))


;;; beside と同じ実装方法
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((painter-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (painter-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (painter-bottom frame)
        (painter-top frame)))))


;;; beside と回転演算を利用した実装
(define (below painter1 painter2)
  (let ((left (rotate270 painter1))
        (right (rotate270 painter2)))
    (rotate90 (beside left right))))


;;; 使用例
((below diamond->painter wave) canvas-frame)

;; 200.0,0.0,0.0,100.0
;; 200.0,0.0,400.0,100.0
;; 0.0,100.0,200.0,200.0
;; 400.0,100.0,200.0,200.0
;; 140.0,370.0,160.0,400.0
;; 260.0,370.0,240.0,400.0
;; 140.0,370.0,160.0,330.0
;; 260.0,370.0,240.0,330.0
;; 240.0,330.0,300.0,330.0
;; 160.0,330.0,120.0,330.0
;; 300.0,330.0,400.0,270.0
;; 240.0,290.0,400.0,230.0
;; 240.0,290.0,300.0,200.0
;; 200.0,260.0,240.0,200.0
;; 120.0,330.0,60.0,320.0
;; 120.0,320.0,60.0,280.0
;; 60.0,320.0,0.0,370.0
;; 60.0,280.0,0.0,330.0
;; 120.0,320.0,140.0,300.0
;; 140.0,300.0,100.0,200.0
;; 200.0,260.0,160.0,200.0

;;; 出力結果を http://sandbox.serendip.ws/sicp_drawing.html の
;;; 上から２つ目のテキストエリアにコピペし、"データ描画"を
;;; クリックして描かれる図形を確認する
