(load "./ex2-86.scm")



; sparse-polymomial パッケージ

(define (install-sparse-polynomial-package)

  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ; 2.3.2 の variable? と same-variable?
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ; adjoin-term ~ coeff
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))


  (define (higher-variable v1 v2)
    (let ((var-order '(y x z)))
      (define (iter l)
        (cond ((equal? v1 (car l)) v1)
              ((equal? v2 (car l)) v2)
              (else (iter (cdr l)))))
      (iter var-order)))
  ; 優先する変数を変えたいときは var-order を変更する

  (define (switch-variable v to-v p)
    (define (iter terms)
      (if (null? terms)
          (make-poly to-v (the-empty-termlist))
          (let ((o (order (car terms)))
                (c (coeff (car terms))))
            (if (equal? (type-tag c) 'sparse-polynomial)
                (add-poly
                  (make-poly
                    to-v
                    (mul-term-by-all-terms
                      (make-term 0
                                 (make-poly v (list (make-term o 1))))
                      (term-list (contents c))))
                  (iter (cdr terms)))
                (add-poly
                  (make-poly
                    to-v
                    (list (make-term 0
                                     (attach-tag 'sparse-polynomial
                                                 (make-poly v (list (make-term o c)))))))
                  (iter (cdr terms)))))))
    (iter (term-list (contents p))))


  ; 多項式の加算
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
  ;    (error "Polys not in same var -- ADD-POLY"
  ;           (list p1 p2))))
        (let ((v1 (variable p1)) (v2 (variable p2)))
          (if (equal? (higher-variable v1 v2) v1)
              (add-poly p1 (switch-variable v2 v1 p2))
              (add-poly p2 (switch-variable v1 v2 p1))))))

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

  (define (reverse-sign term)
    (make-term (order term)
               (* (coeff term) -1)))

  ; 多項式の乗算
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
  ;    (error "Polys not in same var -- MUL-POLY"
  ;           (list p1 p2))))
      (let ((v1 (variable p1)) (v2 (variable p2)))
        (if (equal? (higher-variable v1 v2) v1)
            (mul-poly p1 (switch-variable v2 v1 p2))
            (mul-poly p2 (switch-variable v1 v2 p1))))))

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
         (make-term (add (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (=zero?-iter L)
    (or (empty-termlist? L)
       (and (=zero? (coeff (first-term L)))
            (=zero?-iter (rest-terms L)))))

  ; 多項式の除算
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (sub (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms
                         (add-terms L1
                                    (map reverse-sign
                                         (mul-term-by-all-terms (make-term new-o new-c)
                                                                L2)))
                         L2)))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))



  ; dense->sparse
  (define (dense->sparse p)
    (let ((terms (term-list p)))
      (define (iter terms)
        (if (null? terms)
            '()
            (append (list (list (- (length terms) 1) (car terms)))
                    (iter (cdr terms)))))
      (make-poly (variable p) (iter terms))))

  ; scheme-number->sparse-polynomial
  (define (scheme->sparse s p)
    (make-poly (variable p)
               (list (make-term 0 s))))


  ; 多項式(の項リスト)用の gcd 演算
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  ; remainder-terms は、div-terms の結果から剰余の項リストのみを取り出す
  (define (remainder-terms a b)
    (cadr (div-terms a b))) 

  ; gcd-poly
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))


  ; 他の部分とのインターフェース
  (define (tag p) (attach-tag 'sparse-polynomial p))
  (put 'add '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'add '(sparse-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (dense->sparse p2)))))
  (put 'add '(dense-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (add-poly (dense->sparse p1) p2))))
  (put 'sub '(sparse-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 (dense->sparse p2)))))
  (put 'sub '(dense-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (sub-poly (dense->sparse p1) p2))))
  (put 'mul '(sparse-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 (dense->sparse p2)))))
  (put 'mul '(dense-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (mul-poly (dense->sparse p1) p2))))

  (put 'div '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  (put 'make 'sparse-polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(sparse-polynomial)
       (lambda (p) (=zero?-iter (term-list p))))

  (put 'gcd '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  ; sparse-polynomial と scheme-number 間での演算
  (put 'add '(sparse-polynomial scheme-number)
       (lambda (p s) (tag (add-poly p (scheme->sparse s p)))))
  (put 'add '(scheme-number sparse-polynomial)
       (lambda (s p) (tag (add-poly (scheme->sparse s p) p))))
  (put 'mul '(sparse-polynomial scheme-number)
       (lambda (p s) (tag (mul-poly p (scheme->sparse s p)))))
  (put 'mul '(scheme-number sparse-polynomial)
       (lambda (s p) (tag (mul-poly (scheme->sparse s p) p))))
  'done)


(install-sparse-polynomial-package)



(define (make-polynomial var terms)
  (if (number? (car terms))
      ((get 'make 'dense-polynomial) var terms)
      ((get 'make 'sparse-polynomial) var terms)))
 


; 汎用演算 greatest-common-divisor
(define (greatest-common-divisor a b)
  (if (equal? (type-tag a) 'sparse-polynomial)
      (apply-generic 'gcd a b)
      (gcd a b)))




; テスト
(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13 (0 5)))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(print p1)
(print (greatest-common-divisor q1 q2))
