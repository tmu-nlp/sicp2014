(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= 1 (length leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set
        (make-code-tree (car leaf-set)
                        (cadr leaf-set))
        (cddr leaf-set)))))


;;; 使用例

;;; n = 5
(define pairs1 '((A 1) (B 2) (C 4) (D 8) (E 16)))
(generate-huffman-tree pairs1)
;;; (((((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7) (leaf D 8) (A B C D) 15) (leaf E 16) (A B C D E) 31)

;;; n = 10
(define pairs2 '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
(generate-huffman-tree pairs2)
;;; ((((((((((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7) (leaf D 8) (A B C D) 15) (leaf E 16) (A B C D E) 31) (leaf F 32) (A B C D E F) 63) (leaf G 64) (A B C D E F G) 127) (leaf H 128) (A B C D E F G H) 255) (leaf I 256) (A B C D E F G H I) 511) (leaf J 512) (A B C D E F G H I J) 1023)

;;; 最高頻度: 1 bit
;;; 最低頻度: n-1 bit
