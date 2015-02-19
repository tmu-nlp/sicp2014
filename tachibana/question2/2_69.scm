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

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (enc-iter tree)
    (if (leaf? tree)
        '()
        (if (memq symbol (symbols (left-branch tree)))
            (cons 0 (enc-iter (left-branch tree)))
            (cons 1 (enc-iter (right-branch tree))))))
  (if (memq symbol (symbols tree))
      (enc-iter tree)
      (error "Not Found symbol of " symbol)))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; 記号
                               (cadr pair))  ; 頻度
                    (make-leaf-set (cdr pairs))))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; adjoin-setは木や葉を重さの昇順に並び替える
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; ans
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
        (adjoin-set
          (make-code-tree (car leaf-set) (cadr leaf-set))
          (cddr leaf-set)))))

(print (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1) (E 6) (F 8))))

(print (make-leaf-set '((A 4) (B 2) (C 1) (D 1) (E 6) (F 8))))
(print (car (make-leaf-set '((A 4) (B 2) (C 1) (D 1) (E 6) (F 8)))))
(print (cadr (make-leaf-set '((A 4) (B 2) (C 1) (D 1) (E 6) (F 8)))))
(print (cddr (make-leaf-set '((A 4) (B 2) (C 1) (D 1) (E 6) (F 8)))))

; make-leaf-setでleafを重みの小さい順に並び替えて、小さいものの二つから木を作り、そのあと残っている
; 木と葉を重みが小さい順に並び替えて再帰させて、一つの木になるまでそれを行う.








