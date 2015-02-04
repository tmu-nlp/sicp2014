;ex2_44
(define wave einstein)
(paint wave)

;beside 二つのペインタをとり, 第一のペインタの画像をフレームの左半分に描き, 第二のペインタの画像をフレームの右半分に描く新しい合成ペインタを作り出す
(define wave2 (beside wave (flip-vert wave)))
(paint wave2)

;below 二つのペインタをとり, 第一のペインタの画像を第二のペインタの画像の下に描く合成ペインタを作る
(define wave4 (below wave2 wave2))
(paint wave4)

;letは局所関数
;(let ((i 1) (j 2))
;    (+ i j))

;wave4の抽象化 
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))
(paint wave4)


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split wave 4))


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split wave 4))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split wave 4))


;corner-split の四つコピーを配置
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit wave 4))



(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


;四つの一引数ペインタ演算をとり, 与えられたペインタをこの四つの演算で変換し, 結果を四角の中に配置するペインタ演算を作り出す. 
;tl, tr, blおよびbrはそれぞれ上左コピー, 上右コピー, 下左コピーおよび下右コピーに作用させる変換
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter))) 
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(paint (identity wave))
(paint (flip-vert wave))
(paint (flip-horiz wave))
;以下は一緒
(paint (flip-vert (flip-horiz wave)))
(paint (rotate180 wave))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(paint (flipped-pairs wave))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit wave 4))
