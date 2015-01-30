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

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


;;; 使用例
((flip-horiz wave) canvas-frame)
;; 260.0,340.0,240.0,400.0
;; 140.0,340.0,160.0,400.0
;; 260.0,340.0,240.0,260.0
;; 140.0,340.0,160.0,260.0
;; 160.0,260.0,100.0,260.0
;; 240.0,260.0,280.0,260.0
;; 100.0,260.0,0.0,140.0
;; 160.0,180.0,0.0,60.0
;; 160.0,180.0,100.0,0.0
;; 200.0,120.0,160.0,0.0
;; 280.0,260.0,340.0,240.0
;; 280.0,240.0,340.0,160.0
;; 340.0,240.0,400.0,340.0
;; 340.0,160.0,400.0,260.0
;; 280.0,240.0,260.0,200.0
;; 260.0,200.0,300.0,0.0
;; 200.0,120.0,240.0,0.0

;;; 出力結果を http://sandbox.serendip.ws/sicp_drawing.html の
;;; 上から２つ目のテキストエリアにコピペし、"データ描画"を
;;; クリックして描かれる図形を確認する

((rotate180 wave) canvas-frame)
;; 260.0,60.0,240.0,0.0
;; 140.0,60.0,160.0,0.0
;; 260.0,60.0,240.0,140.0
;; 140.0,60.0,160.0,140.0
;; 160.0,140.0,100.0,140.0
;; 240.0,140.0,280.0,140.0
;; 100.0,140.0,0.0,260.0
;; 160.0,220.0,0.0,340.0
;; 160.0,220.0,100.0,400.0
;; 200.0,280.0,160.0,400.0
;; 280.0,140.0,340.0,160.0
;; 280.0,160.0,340.0,240.0
;; 340.0,160.0,400.0,60.0
;; 340.0,240.0,400.0,140.0
;; 280.0,160.0,260.0,200.0
;; 260.0,200.0,300.0,400.0
;; 200.0,280.0,240.0,400.0

((rotate270 wave) canvas-frame)
;; 340.0,260.0,400.0,240.0
;; 340.0,140.0,400.0,160.0
;; 340.0,260.0,260.0,240.0
;; 340.0,140.0,260.0,160.0
;; 260.0,160.0,260.0,100.0
;; 260.0,240.0,260.0,280.0
;; 260.0,100.0,140.0,0.0
;; 180.0,160.0,60.0,0.0
;; 180.0,160.0,0.0,100.0
;; 120.0,200.0,0.0,160.0
;; 260.0,280.0,240.0,340.0
;; 240.0,280.0,160.0,340.0
;; 240.0,340.0,340.0,400.0
;; 160.0,340.0,260.0,400.0
;; 240.0,280.0,200.0,260.0
;; 200.0,260.0,0.0,300.0
;; 120.0,200.0,0.0,240.0
