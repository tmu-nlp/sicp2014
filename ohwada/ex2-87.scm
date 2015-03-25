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
;------------------- 追加部分-------------------------
  (define (=zero?-iter L)
    (or (empty-termlist? L)
       (and (=zero? (coeff (first-term L)))
            (=zero?-iter (rest-terms L)))))

  ; scheme-number->polynomial
  (define (scheme->poly s p)
    (make-poly (variable p)
               (list (make-term 0 s))))

;-----------------------------------------------------

  ; 他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

;-------------------- 追加部分---------------------------------------
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-iter (term-list p))))

  ; polynomial と scheme-number 間での演算
  (put 'add '(polynomial scheme-number)
       (lambda (p s) (tag (add-poly p (scheme->poly s p)))))
  (put 'add '(scheme-number polynomial)
       (lambda (s p) (tag (add-poly (scheme->poly s p) p))))
  (put 'mul '(polynomial scheme-number)
       (lambda (p s) (tag (mul-poly p (scheme->poly s p)))))
  (put 'mul '(scheme-number polynomial)
       (lambda (s p) (tag (mul-poly (scheme->poly s p) p))))
;-----------------------------------------------------------------------

  'done)


(install-polynomial-package)




(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))





(define A (make-polynomial 'x '((2 7) (1 2) (0 4))))
(define B (make-polynomial 'y '((2 3) (0 1))))
(print A)
(print B)
;(print (=zero? (make-polynomial 'x '((0 0)))))
;(print (=zero? (make-polynomial 'x '((0 4)))))

(define C (make-polynomial 'x (list (list 2 B) (list 0 3))))
(print C)
(print (add A C))

