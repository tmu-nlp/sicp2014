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



(print (union-set (list 3 6 4 1 5) (list 4 9 2 1))) ; (3 6 4 1 5 4 9 2 1)
(print (intersection-set (list 1 3 3 3 2) (list 3 3))) ; (3 3)

; adjoin と union が効率的に処理できるようになる(adjoin はΘ(1), union はΘ(n))
; union の append はΘ(n)だが、2.59 の union は element-of-set? (Θ(n)) を set1 の 
; 要素数回呼び出すのでステップ数は Θ(n^2)。従ってこちらの方が速い

;要素数が増えるので element-of-set? は遅くなる → intersection は更に遅くなる
 
