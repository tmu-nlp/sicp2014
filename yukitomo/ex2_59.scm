;ex2_59.scm

;与えられた要素が集合の構成要素であるか
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;引数としてオブジェクトと集合をとり, 元の集合の要素と追加する要素を含む集合
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;二つの集合の積集合, つまり両方の集合に現れる要素だけを含む集合を計算
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;answer
(define (union-set set1 set2)
  (cond ((null? set1) set2)
  		((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(display (union-set '(1 2 3 4 5) '(2 3 6)))
(display "\n")
(display (union-set '(1 2 3 4 5) '()))

(display (union-set '() '(1 2 3 4 5)))