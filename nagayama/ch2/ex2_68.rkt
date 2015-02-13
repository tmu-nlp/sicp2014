#lang racket

; 
; encode 手続きを用いて, 実際に符号化する
; encode-symbol 手続きを定義する
; 存在しないシンボルだった場合は error を返すようにする
; 

; encode : 符号化
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; encode-symbol
(define (encode-symbol char tree)
  (define (branch-correct? branch)
    (if (leaf? branch)
        (equal? char (symbol-leaf branch))
        (element-of-set? char (symbols branch))))
  (let ((lb (left-branch tree))
        (rb (right-branch tree)))
    (cond ((branch-correct? lb)
           (if (leaf? lb) '(0) (cons 0 (encode-symbol char lb))))
          ((branch-correct? rb)
           (if (leaf? rb) '(1) (cons 1 (encode-symbol char rb))))
          (else (error "bad symbol: ENCODE-SYMBOL" char)))))

; 存在するシンボルかの判定
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))



; セレクタなど
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; decode : 複合化手続き
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


; sample-tree
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

;; 出力
(define test-mes '(B A D A C))
(define test-code (encode test-mes sample-tree))
(define test-res  (decode test-code sample-tree))

(display "test-mes  : ")
test-mes
(display "test-code : ")
test-code
(display "test-res  : ")
test-res

#|

test-mes  : '(B   A D     A C    )
test-code : '(1 0 0 1 1 0 0 1 1 1)
test-res  : '(B   A D     A C    )

; |#

