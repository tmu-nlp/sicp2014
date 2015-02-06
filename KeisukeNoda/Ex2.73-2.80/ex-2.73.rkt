#lang racket

(define h (make-hash))
(define (put op type element)
  (dict-set! h (list op type) element))
(define (get op type)
  (dict-ref h (list op type) false))


(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Incorrectly marked data -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Incorrectly marked data -- CONTENTS" datum)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for this type -- APPLY-GENERIC"
           (list op type-tags))))))
;-----------------------------------------------------------
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))



(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (if (not (empty? (cdddr p)))
      (make-product (caddr p) (cadddr p))
      (caddr p)))


;-----------------------------------------------------------
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;------------- a ------------------

; ((get 'deriv (operator exp)) (operands exp) var)
; ↑ 最初のプログラムとの違い

; 型である演算子記号(+, *)と、演算である deriv から、対応する演算を選び実行するが、
; number? と、variable? は引数がリストでなく、型を持たないから、データ主導の振り分けに組み込めない。

;------------- b ------------------

;微分のパッケージ
(define (install-deriv-package)
  ;内部手続き
  ;構成子
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  ;選択子
  (define (multiplier opds) (car opds))
  (define (multiplicand opds) (cadr opds))  
  
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  
  ; sum
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  ; exponentiation
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 1) base)
          ((=number? exponent 0) 1)
          (else (list '** base exponent))))
  
  (define (base x)
    (car x))
  
  (define (exponent x)
    (cadr x))
  
  (define (deriv-exponentiation exp var)
    (make-product
     (make-product (exponent exp)
                   (make-exponentiation (base exp)
                                        (- (exponent exp) 1)))
     (deriv (base exp) var)))
  
  ;セットアップ
  ; (get 'deriv (operator exp)) (operands exp) のとき
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  
  ; (get (operator exp) 'deriv) (operands exp)　のとき
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product)
  (put '** 'deriv deriv-exponentiation)
  'done)


(install-deriv-package)

(deriv '(+ (* 3 x) y) 'x)
; 3
(deriv '(** (+ (* 3 x) y) 2) 'x)
; 9

;------------- d ------------------

; 各手続きの put の引数の"演算"と"型"を逆にする。