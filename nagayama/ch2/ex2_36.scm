#lang scheme

; accumulate-n
; 列の列を引数に取り, 各列のk番目の要素毎にaccumulateし
; 結果の列を返す手続き

; n-集積
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


; 集積
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; run
(define s (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))
(accumulate-n + 0 s)

