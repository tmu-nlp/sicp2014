#!/usr/bin/gosh

;あるかどうかの確認を行わないのでelement-of-set?は必要なし
;(define (element-of-set? x set)
;  (cond ((null? set) #f)
;        ((equal? x (car set)) #t)
;        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))
;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))


(define (union-set set1 set2) (append set1 set2))
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;  	    ((null? set2) set1)
;        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
;        (else (cons (car set1) (union-set (cdr set1) set2)))))

#?=(union-set '(1 2 3 4 5) '(2 3 6))
#?=(adjoin-set '(1 2 3 4 5) '(2 3 6))

; ex2_59と比べ、毎回のオブジェクトの検索(element-of-set?)が減るため効率がよくなる
; 応用はいろいろ