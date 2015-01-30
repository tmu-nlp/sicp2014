; #lang scheme

; if文の処理順の特殊性.


(define (new-if predicate then-clause else-clause)
        (cond (predecate then-clause)
              (else else-clause)))

(define (sqrt-iter guess x)
        (new-if (good-enough? guess x)
                guess
                (sqrt-iter (improve guess x) x)))

; if文と違い、new-if関数は条件文の真偽に関わらず
; 第2引数と第3引数を評価するため無限ループが発生する。 
