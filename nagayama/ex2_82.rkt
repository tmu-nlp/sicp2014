#lang racket

; apply-generic を可変長引数が使えるようにする.
; 
; 可変長引数の型をそろえる
; 
; 手法
;  全ての引数を 第1引数 の型に
;  全ての引数を 第2引数 の型に
;   ...
;  全ての引数を 第N引数 の型に
; と再帰的に強制型変換を試みる.
; 
; 


(define (apply-generic op . args)
  ;引数を受け取り、該当する引数に適した手続きを返却する
  (define (get-proc args)
    (let ((type-tags (map type-tag args)))
      (get op type-tags)))

  ;指定した型タグを使ってargsリストのデータを強制型変換する。
  (define (convert-types-by target-type-tag args)
    (map (lambda (arg)
       (if (eq? target-type-tag (type-tag arg))
           ;型タグが等しければそのままデータを返却
           arg
           ;異なる型タグだった場合は強制型変換処理を行う。
           (let ((proc (get-coercion (type-tag arg) target-type-tag)))
         (if proc
             ;該当する強制型変換手続きが存在した場合は変換したデータを返却
             (proc arg)
             ;手続きが存在しなかった場合はデータをそのまま返却
             arg))))
     args))

  ;強制型変換を行い、その結果をリストで返却する。
  (define (convert-types args)
    (let loop ((dst args)  ;型変換後の結果
           (index 0))  ;どの引数まで型変換が進んだか。
      (if (< index (length dst))
      ;インデックス内の場合は変換後のdstで、指定要素の型を使って変換を行う。
      (loop (convert-types-by (type-tag (ref dst index)) dst)
        (+ 1 index))
      ;最後の要素まで変換してきた場合は最終変換結果を返却する
      dst)))

  ;メイン処理。まずは受け取った引数でそのままチェック。
  (let ((proc (get-proc args)))
    (if proc
    ;ヒットしたらそれを適用
    (apply proc (map contents args))
    ;ヒットしなかったら強制型変換して適用可能な手続きを探す。
    (let ((args1 (convert-types args)))
      (let ((proc1 (get-proc args1)))
        (if proc1
            (apply proc1 (map contents args1))
            (error "No method for these types"
               (list op type-tags))))))))


