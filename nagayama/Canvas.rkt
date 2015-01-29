;;;; Canvas.rkt

(module common racket
  
  ; reauire
  (require graphics/graphics)
  
  ; キャンバスサイズの設定
  (define canvas-margin 4)
  (define canvas-width  512)
  (define canvas-height 512)
  
  ;; view point
  (define vp (list))
  
  ; open-canvas : 窓を開く
  (define open-canvas
    (lambda ()
      (if (null? vp)
          (begin
            (open-graphics)
            (set! vp (open-viewport "The Picture Language"
                                    (+ canvas-width  (* canvas-margin 2))
                                    (+ canvas-height (* canvas-margin 2)))))
          (list))))
  
  ; close-canvas : 窓を閉じる
  (define close-canvas
    (lambda ()
      (if (null? vp)
          (list)
          (begin
            (close-viewport vp)
            (close-graphics)
            (set! vp (list))))))
  
  ; clear-canvas : 
  (define clear-canvas
    (lambda ()
      (if (null? vp)
          (list)
          ((clear-viewport vp)))))
)