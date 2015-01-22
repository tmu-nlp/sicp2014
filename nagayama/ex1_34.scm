; #lang scheme

; (f f) について 説明する.

; main
(define (f g) (g 2))

; run
(f f)

; (f f) を評価したら
;
;    (f f) を受けて...
; -> (g 2) の g に f を代入 -> (f 2)
; -> (g 2) の g に 2 を代入 -> (2 2)
; -> "2" という手続きは用意されていないため error

; 



