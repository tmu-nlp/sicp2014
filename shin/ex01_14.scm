#!/usr/bin/gosh

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0) 
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination 
                         kinds-of-coins))
                     kinds-of-coins))))) 
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

#?=(count-change 1000)

;時間計算量n^2
;空間計算量n^2
;(cc 11 5)を計算するのに一番深くまで計算しないといけないから
;深さnで幅がnの三角形になって(n^2)/2だけどオーダーはn^2
