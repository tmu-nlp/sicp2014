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

  (define (reverse-sign term)
    (make-term (order term)
               (* (coeff term) -1)))

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


  ; dense->sparse
  (define (dense->sparse p)
    (let ((terms (term-list p)))
      (define (iter terms)
        (if (null? terms)
            '()
            (append (list (list (- (length terms) 1) (car terms)))
                    (iter (cdr terms)))))
      (make-poly (variable p) (iter terms))))

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


  (put 'make 'sparse-polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(sparse-polynomial)
       (lambda (p) (=zero?-iter (term-list p))))
  'done)


(install-sparse-polynomial-package)



;------------------------------------------------------------------

; dense-polynomial パッケージ

(define (install-dense-polynomial-package)

  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ; adjoin-term ~ coeff
  (define (adjoin-term term term-list)
    (cons term term-list))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))


  (define (order term-list) (length (rest-terms term-list)))
  (define (coeff term) term)


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
            (cond ((> (order L1) (order L2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order L1) (order L2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (add (coeff t1) (coeff t2))
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
     (* (coeff term) -1))



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
      (add-terms (mul-term-by-all-terms (first-term L1) (order L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 O1 L)
    (define (iter t L)
      (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term
              (mul (coeff t) (coeff t2))
              (iter t (rest-terms L))))))
    (define (make-0-coeff-list O)
      (if (= O 0)
          '()
          (cons 0 (make-0-coeff-list (- O 1)))))
    (append (iter t1 L) (make-0-coeff-list O1)))


  (define (=zero?-iter L)
    (or (empty-termlist? L)
       (and (=zero? (coeff (first-term L)))
            (=zero?-iter (rest-terms L)))))


  ; sparse->dense
  (define (sparse->dense p)
    (let ((terms (term-list p)))
      (define (iter terms O)
        (cond ((null? terms) '())
              ((not (= (caar terms) O))
               (cons 0 (iter (cdr terms) (- O 1))))
              (else
               (cons (cadar terms) (iter (cdr terms) (- O 1))))))
      (make-poly (variable p) (iter terms (caar terms)))))


  ; 他の部分とのインターフェース
  (define (tag p) (attach-tag 'dense-polynomial p))
  (put 'add '(dense-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(dense-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(dense-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'make 'dense-polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(dense-polynomial)
       (lambda (p) (=zero?-iter (term-list p))))
  'done)


(install-dense-polynomial-package)


;---------------------------------------------------------------

(define (make-polynomial var terms)
  (if (number? (car terms))
      ((get 'make 'dense-polynomial) var terms)
      ((get 'make 'sparse-polynomial) var terms)))
 
;---------------------------------------------------------------


; テスト
(define A (make-polynomial 'x '((2 2) (0 5))))
(define B (make-polynomial 'x '(3 4 1)))

(print A)
(print B)

(print (add A B))
(print (sub A B))
(print (mul A B))

