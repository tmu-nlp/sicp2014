;;; append-map を利用するため
(use srfi-1)

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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (contains? symbol lst)
  (if (member symbol lst)
      #t
      #f))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? symbol (symbol-leaf tree)))
         '())
        ((contains? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol: " symbol))))


;;; 使用例
(define pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define rock-tree (generate-huffman-tree pairs))

(define rock-song
  (list '(GET A JOB)
        '(SHA NA NA NA NA NA NA NA NA)
        '(GET A JOB)
        '(SHA NA NA NA NA NA NA NA NA)
        '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP)
        '(SHA BOOM)))

(append-map (lambda (sentence) (encode sentence rock-tree)) rock-song)
;;; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0
;;;  0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
;;;  0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)


;;; 符号化に何ビット必要か？
(length (append-map (lambda (sentence) (encode sentence rock-tree)) rock-song)) ; 84

;;; 84ビット


;;; 固定長符号での最小ビットは？
(apply + (map length rock-song))        ; 36
(* 3 36)                                ; 108

;;; 108 ビット
