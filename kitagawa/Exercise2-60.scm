(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))


;;; 使用例
(define set1 (list 2 3 2 1 3 2 2))
(define set2 (list 4 5 1 2 4 2 1))

(element-of-set? 1 set1)                ; #t
(element-of-set? 5 set1)                ; #f

(adjoin-set 1 set1)                     ; (1 2 3 2 1 3 2 2)
(adjoin-set 5 set1)                     ; (5 2 3 2 1 3 2 2)

(intersection-set set1 set2)            ; (2 2 1 2 2)
(intersection-set set1 '())             ; ()

(union-set set1 set2)                   ; (2 3 2 1 3 2 2 4 5 1 2 4 2 1)
(union-set set1 '())                    ; (2 3 2 1 3 2 2)


;;; 効率について
;;;
;;; element-of-set? : 全ての要素を探索するのに時間がかかるようになる
;;; adjoin-set      : 要素が集合に含まれるか判定する必要がなくなり、高速化する
;;; intersection-set: element-of-set? の影響で時間がかかるようになる
;;; union-set       : 要素が集合に含まれるか判定する必要がなくなり、高速化する

;;; 応用について
;;; adjoin-set, union-set の使用頻度が高い集合データを扱う場合は、
;;; 処理が高速化するので"重複あり集合"の方が良い。
