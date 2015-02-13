(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? object)
    (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left)
                  (symbols right))
          (+ (weight left)
             (weight right))))

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
          (else (error "bad bit: CHOOSE-CRANCH" bit))))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set)))
            (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                                   (cadr pair))
                        (make-leaf-set (cdr pairs))))))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (cond
        ((leaf? tree) '())
        ((include? symbol (symbols tree))
            (if (include? symbol (symbols (left-branch tree)))
                (append (list 0)
                        (encode-symbol symbol (left-branch tree)))
                (append (list 1)
                        (encode-symbol symbol (right-branch tree)))))
        (else (error "bad symbol: ENCODE-SYMBOL" symbol))))

(define (include? s symbols)
    (if (pair? (memq s symbols))
        #t
        #f))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
    (if (= (length pairs) 1)
        (car pairs)
        (let ((first (car pairs))
              (second (cadr pairs))
              (rest (subset pairs 2)))
            (successive-merge (adjoin-set (make-code-tree first second)
                                          rest)))))

(define (subset set n)
    (if (= n 0)
        set
        (subset (cdr set) (- n 1))))

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                        (make-leaf 'B 2)
                        (make-code-tree (make-leaf 'D 1)
                                        (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define set (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(define set2 (list (list 'B 2) (list 'D 1) (list 'A 4) (list 'C 1)))
(define set3 '(('A 2) ('B 2) ('C 3) ('D 3) ('E 4)))

(define huffman-tree (generate-huffman-tree set))
(define huffman-tree2 (generate-huffman-tree set2))

(print huffman-tree)
(print huffman-tree2)

(print (decode sample-message huffman-tree))
(print (decode sample-message huffman-tree2))
(print (generate-huffman-tree set3))
