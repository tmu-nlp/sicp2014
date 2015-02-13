#lang racket


; ex2.68 で作成した符号化手続きについて,
; 符号化するのに必要なステップ数の増加のオーダーはいくらか.
; 
; また, シンボルの相対頻度が特別な場合の
; 最大頻度のシンボルと最小頻度のシンボルを
; 符号化する際に必要になるステップ数の増加のオーダーはいくらか.

#|
message : N 文字数
tree    : M 文字の種類

(encode N M) -> (encode-symbol A tree) * N

(encode-symbol A M) -> (element-of-set? A set) * (M-k)

    
    
最大頻度 : O(1)
最小頻度 : O(N^2)

; |#
