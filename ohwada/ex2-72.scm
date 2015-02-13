; 葉の constructor, predicate, selector
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))


(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


; 木の constructor, selector
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


; 復号化の手続き
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


; 符号化木とメッセージを定義
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


; 符号化の手続き
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))



(define (encode-symbol symbol tree)
   (cond ((leaf? tree) '())
         ((element-of-set? symbol (symbols (left-branch tree)))
          (cons '0 (encode-symbol symbol (left-branch tree))))
         ((element-of-set? symbol (symbols (right-branch tree)))
          (cons '1 (encode-symbol symbol (right-branch tree))))
         (else (display "error"))))



; adjoin-set と make-leaf-set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


; Huffman 木を生成する手続き
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set)) (car leaf-set)
      (successive-merge
        (adjoin-set (make-code-tree (car leaf-set)
                                    (cadr leaf-set))
                    (cddr leaf-set)))))



; 2.71

; n = 5
; 相対頻度 → 1, 2, 4, 8, 16
; n = 10
; 相対頻度 → 1, 2, 4, 8, 16, 32, 64, 128, 256, 512

(define pairs-n-5 '((A 16) (B 8) (C 4) (D 2) (E 1)))
(define pairs-n-10 '((A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) (H 4) (I 2) (J 1)))

(define tree-n-5 (generate-huffman-tree pairs-n-5))
(define tree-n-10 (generate-huffman-tree pairs-n-10))



; 2.72

; 記号の相対頻度が 2.71 のものである場合

; 最高頻度の記号 → Θ(1) 
; 最低頻度 → 
; n + (n - 1) + (n - 2) + (n - 3) + ... + 1 = n(n-1) → Θ(n^2)

; (エラーが無いと前提すれば、最初の左の枝に symbol が無ければ他のどこかの枝に
; symbol があることが確定するので、こんなに何度も element-of-set? を回す必要は
; 無い。最適化すれば Θ(n) になる？)
