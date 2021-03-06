(load "./ex2-86.scm")



; polymomial パッケージ

(define (install-polynomial-package)

  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ; 2.3.2 の variable? と same-variable?
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))


;--------------------- 変更部分１--------------------------
; adjoin-term
;  (define (adjoin-term term term-list)
;    (if (=zero? (coeff term))
;      term-list
;      (cons term term-list)))

; (2 0 0 0 0) という term と、(4 1) という term-list が与えられたとき、
; (2 0 0 4 1) という term-list を返すようにする
  (define (adjoin-term term term-list)
    (define (iter a)
      (if (= a 0)
          '()
          (cons 0 (iter (- a 1)))))
    (append (list (coeff term))
            (iter (- (order term) (length term-list)))
            term-list))

;----------------------------------------------------------

  (define (the-empty-termlist) '())
  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))


;---------------------- 変更部分２--------------------------
;  (define (make-term order coeff) (list order coeff))
;  (define (order term) (car term))
;  (define (coeff term) (cadr term))

  (define (order-iter order)
    (if (= order 0)
        '()
        (cons 0 (order-iter (- order 1)))))

  (define (make-term order coeff) (append (list coeff) (order-iter order)))
  (define (order term) (- (length term) 1))
  (define (coeff term) (car term))
;--------------------------------------------------------



  ; 多項式の加算
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))


  (define (add-terms L1 L2)
   (cond ((empty-termlist? L1) L2)
         ((empty-termlist? L2) L1)
         (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))



  ; 多項式の減算
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (map reverse-sign (term-list p2))))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))

;---------------- 変更部分３----------------------------
 ;  (define (reverse-sign term)
 ;    (make-term (order term)
 ;               (* (coeff term) -1)))

  (define (reverse-sign coeff)
    (* coeff -1))
;-------------------------------------------------------

  ; 多項式の乗算
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))


  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))


  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))



  (define (=zero?-iter L)
    (or (empty-termlist? L)
       (and (=zero? (coeff (first-term L)))
            (=zero?-iter (rest-terms L)))))



  ; 他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-iter (term-list p))))
  'done)


(install-polynomial-package)




(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))



; テスト
(define A (make-polynomial 'x '(2 3 5)))
(define B (make-polynomial 'x '(3 0 1)))

(print A)
(print B)
(print (add A B))
(print (sub A B))
(print (mul A B))

