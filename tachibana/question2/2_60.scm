;ans
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) (append set1 set2))

(print (union-set '(1 2 3 4 5) '(2 3 6)))

(print (adjoin-set '1 '(1 2 3 4 5)))

; 重複なし表現の対応する手続きと比べ、オブジェクトを検索する手数が減るため効率はよくなる
; この表現の方が使いたくなる応用はなにかの投票数を数えたりする場合とか？