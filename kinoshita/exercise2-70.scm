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

(define code
    '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define tree (generate-huffman-tree code))

(print tree)

(define message
    '(GET A JOB
      SHA NA NA NA NA NA NA NA NA
      GET A JOB
      SHA NA NA NA NA NA NA NA NA
      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
      SHA BOOM))

(print (encode message tree))

;84bit
;固定長の場合
;8 * 36 = 108[bit]
