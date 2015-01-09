(define us-coins (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount
                        (expect-first-denomination
                            coin-values))
                   (cc (- amount
                          (first-denomination coin-values))
                       coin-values)))))
(define (first-denomination coin-values)
    (car coin-values))
(define (expect-first-denomination coin-values)
    (cdr coin-values))
(define (no-more? coin-values)
    (null? coin-values))

(print (cc 100 us-coins))

;リストの順番は解答に影響を与えない
;リストの最初のコインを使用しないパターンすべてと
;リストの最初のコイン額面を引いた金額に対する両替パターンすべての
;和はコインの額面にかかわらず一定であるから，リストの順番に影響される
;ことはない．
;ただし額面の降順でない場合は計算コストが高くなる

