; パスカルの三角形

; Combination
(define (combination x y)
  (define (iter x y ans)
        (if (not (> y 0 ))
            ans
            (iter (- x 1)
                  (- y 1)
                  (/ (* ans x) y))))
  
  (define (func a b)
    (if (< a (* 2 b))
          (- a b)
          b))
    
  (if (or (< x 0)
          (< y 0)
          (< x y))
      0
      (iter x (func x y) 1)))

; stage 段のパスカルの三角形を出力する
(define (pascal_tri stage)
  (define (iter stage x y)
          (if (< stage y)
              (display "finish")
              (if (> x y)
                  (begin (display "\n")
                         (iter stage 0 (+ y 1)))
                  (begin (display " ")
                         (display (combination y x))
                         (iter stage (+ x 1) y)))))
  (iter stage 0 0))

; pascal_tri 処理フロー    
; 段数を超えているか？
; 　処理終了
; 　右にはみ出しているか？
; 　　改行、次の段へ
; 　　要素を出力して右へ


; 関数の実行
(pascal_tri 4)

; 引数段のパスカルの三角形を出力する。
; 5 段以上だと表示がずれる。要修正。
