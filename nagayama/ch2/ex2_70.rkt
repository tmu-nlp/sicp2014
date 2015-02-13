#lang racket

; ハフマン符号と固定長符号とのビット数の違いを調べる.
; 

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

; adjoin-set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; make-leaf-set
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; ハフマン符号化木
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge
       (adjoin-set
         (make-code-tree (car pairs) (cadr pairs))
         (cddr pairs)))))

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
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

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


; 以下、実行

; 符号化木
(define test-pairs '((A 2) (GET 2) (SHA 3) (WAH 1)
                     (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define sample-tree (generate-huffman-tree test-pairs))


(define test-mes '(GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                   SHA BOOM))
(define test-code (encode test-mes sample-tree))
(define test-res  (decode test-code sample-tree))

(display "test-mes      : ") (display test-mes) (newline)
(display "length (mes)  : ") (length test-mes) (newline)
(display "test-code     : ") (display test-code) (newline)
(display "length (code) : ") (length test-code) (newline)

#|

ハフマン符号で符号化した方は 84bit であった.
元の歌詞は 8種類 36語 であったので,
固定長符号で符号化すると

  3 * 36 = 108 [bit]

必要である.

|#

