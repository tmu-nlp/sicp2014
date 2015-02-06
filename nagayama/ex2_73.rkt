#lang racket

; 記号微分手続きを データ主導型 に書き直す.
; 
; 

#|
[a] 

(deriv '(+ x y) 'x)
を例にする. このプログラムは, 演算子'deriv と識別ラベル'+ の
2つから適当な手続きを検索する.
そして, 見つかった手続きに (x y), 'x を作用させる.

number?, variable?
をデータ主導型で表現できないのは, 
これらの手続きは 識別ラベルを持たない単項演算子だからである. 

; |#

(require r5rs)


; #| put, get
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; |#


; 言い換え, 前提手続き
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? x y)
  (and (number? x) (= x y)))


; deriv
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 
(define (make operator ex1 ex2)
  ((get 'make operator) ex1 ex2))


; [b]
; 加算パッケージ
(define (install-sum-package)
  (define addend car)
  (define augend cdr)
  
  (define (make-sum ex1 ex2)
    (cond ((=number? ex1 0) ex2)
          ((=number? ex2 0) ex1)
          ((and (number? ex1) (number? ex2)) (+ ex1 ex2))
          (else (list '+ ex1 ex2))))
  
  (define (deriv-sum exp var)
    (make '+
          (deriv (addend exp) var)
          (deriv (augend exp) var)))
  
  (put 'deriv '+ deriv-sum)
  (put 'make '+ make-sum))

(install-sum-package)


; [b]
; 乗算パッケージ
(define (install-product-package)
  (define multiplier car)
  (define multiplicand cadr)
  
  (define (make-product ex1 ex2)
    (cond ((or (=number? ex1 0) (=number? ex2 0)) 0)
          ((=number? ex1 1) ex2)
          ((=number? ex2 1) ex1)
          ((and (number? ex1) (number? ex2)) (* ex1 ex2))
          (else (list '* ex1 ex2))))
  
  (define (deriv-product exp var)
    (make '+
          (make '*
                (multiplier exp)
                (deriv (multiplicand exp) var))
          (make '*
                (deriv (multiplier exp) var)
                (multiplicand exp))))
  
  (put 'deriv '* deriv-product)
  (put 'make '* make-product))

(install-product-package)


; [c]
; 冪乗パッケージ
(define (install-exponentiation-package)
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  
  (define (make-exponentiation ex1 ex2)
    (cond ((=number? ex2 0) 1)
          ((=number? ex2 1) ex1)
          ((number? ex1) (number? ex2) (expt ex1 ex2))
          (else (list '** ex1 ex2))))
  
  (define (deriv-exponentiation exp var)
    (make '*
          (make '*
                (exponent exp)
                (make '**
                      (base exp)
                      (make '+ (exponent exp) -1)))
          (deriv (base exp) var)))
  
  (put 'deriv '** deriv-exponentiation)
  (put 'make '** make-exponentiation))

(install-exponentiation-package)


; run
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (** x 3) y) 'x)
(deriv '(** x 3) 'x)


; [d]
; dirv の手続き呼び出し部を
; 
; ((get (oprrator exp) 'deriv) (operands exp) var)
; 
; に書き換えると, 
; 各パッケージの
; 
