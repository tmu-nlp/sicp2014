#lang scheme

; 集積としてのリスト操作命令の定義
; 空欄補完


(define (map p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))    ; <??>
   (list)
   sequence))

(define (append seq1 seq2)
  (accumulate
   cons
   seq2    ; <??>
   seq1    ; <??>
   ))

(define (length sequence )
  (accumulate 
   (lambda (x y) (+ y 1))    ; <??>
   0
   sequence))

; 集積
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


