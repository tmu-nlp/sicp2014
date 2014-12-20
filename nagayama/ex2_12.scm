#lang scheme

; 以下の手続きの定義
; make-center-percent : 中央値と許容誤差(%)から区間を生成する手続き
; percent             : 区間 から許容誤差(%)を算出する

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))


; 以下, テキストより
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (- (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))



