#lang scheme

; section 1.2.2 両替パターンの数え上げを
; リストを用いた手続きで書き直す

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

; coin-values
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; 
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

; run
(cc 100 us-coins)

; コインの種類の並び順は結果に影響を与えない
; 
; この数え上げアルゴリズムは
; 
;      [金額]を[n]種類のコインで支払うパターン
;  =   [金額]を[n-1]種類のコインで支払うパターン
;    + [より少ない金額]を[n]種類のコインで支払うパターン
; 
; という考えが元になった全探査アルゴリズムである. 
; コインの額面が正でさえあれば何枚目のコインがいくらかは関係ない.
; 
