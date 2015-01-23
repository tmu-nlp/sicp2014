#lang racket

(require graphics/graphics)

; --- ベクトル --- 
(define (make-vect x y)
  (make-posn x y))

(define (xcor-vect v)
  (posn-x v))

(define (ycor-vect v)
  (posn-y v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; --- セグメント ---
(define (make-segment start end)
  (list start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))

; --- フレーム ---
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
 
;---------------------------------
(open-graphics)
(define w (open-viewport "painter" 300 300))

; --- ペインタ ---
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       ((draw-line w)
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(define frame (make-frame (make-vect 275 275) (make-vect -250 0) (make-vect 0 -250)))

; ------- wave -------
(define (connect vect-list)
  (define (iter segment-list remaining)
    (if (null? (cdr remaining))
        (reverse segment-list)
        (iter (cons (make-segment (car remaining)
                                     (cadr remaining))
                       segment-list)
                 (cdr remaining))))
  (iter null vect-list))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) 
                     (make-vect 1.0 1.0) 
                     (make-vect 0.0 0.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; --- flip-horiz ---
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


;; --- rotate(180) ---
(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

;; --- rotate(270) ---
(define (rotate270 painter)
  (rotate90 (rotate90 (rotate90 painter))))

;; --- beside ---
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point
                              ))
          (paint-down
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(define (split t1 t2)
  (lambda (painter n)
    (if (= n 0)
        painter
      (let ((smaller ((split t1 t2) painter (- n 1))))
        (t1 painter (t2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

; =======================================================
(define painterB (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                                          (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define wave  
  (segments->painter (append (connect (list (make-vect 0.4  0.0)
                                             (make-vect 0.5  0.33)
                                             (make-vect 0.6  0.0))) ;股下
                              (connect (list (make-vect 0.25 0.0)
                                             (make-vect 0.33 0.5)
                                             (make-vect 0.3  0.6)
                                             (make-vect 0.1  0.4)
                                             (make-vect 0.0  0.6))) ;左下
                              (connect (list (make-vect 0.0  0.8)
                                             (make-vect 0.1  0.6)
                                             (make-vect 0.33 0.65)
                                             (make-vect 0.4  0.65)
                                             (make-vect 0.35 0.8)
                                             (make-vect 0.4  1.0))) ;左上
                              (connect (list (make-vect 0.75 0.0)
                                             (make-vect 0.6  0.45)
                                             (make-vect 1.0  0.15)));右下
                              (connect (list (make-vect 1.0  0.35)
                                             (make-vect 0.8  0.65)
                                             (make-vect 0.6  0.65)
                                             (make-vect 0.65 0.8)
                                             (make-vect 0.6  1.0))) ;右上
                              (connect (list (make-vect 0.4  0.80)
                                             (make-vect 0.4  0.75)
                                             (make-vect 0.6  0.75)
                                             (make-vect 0.6  0.80))))));笑顔

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner)))))
; ((corner-split wave 1) frame)

(define (square-limit painter n)
  (let ((combine4 (square-of-four rotate90 rotate270
                                  rotate270 rotate90)))
    (combine4 (corner-split painter n))))

((square-limit wave 5) frame)